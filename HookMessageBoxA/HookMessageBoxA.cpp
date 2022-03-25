#include "stdafx.h"

#define DEVICE_NAME L"\\Device\\MsgHooker"
#define SYMBOLICLINE_NAME L"\\??\\MsgHooker"  //ring3用CreateFile打开设备时,用"\\\\.\\MyTestDriver"//相当于起的别名

#define OPER1 CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define OPER2 CTL_CODE(FILE_DEVICE_UNKNOWN,0x900,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define OPER_SETINT CTL_CODE(FILE_DEVICE_UNKNOWN,0x845,METHOD_BUFFERED,FILE_ANY_ACCESS)

void HookMessageBoxAUnload(IN PDRIVER_OBJECT DriverObject);

NTSTATUS HookMessageBoxACreateClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

NTSTATUS HookMessageBoxADefaultHandler(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

NTSTATUS HookMessageBoxAAddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT PhysicalDeviceObject);

NTSTATUS HookMessageBoxAPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);

typedef struct _deviceExtension {
    PDEVICE_OBJECT DeviceObject;
    PDEVICE_OBJECT TargetDeviceObject;
    PDEVICE_OBJECT PhysicalDeviceObject;
    UNICODE_STRING DeviceInterface;
} HookMessageBoxA_DEVICE_EXTENSION, *PHookMessageBoxA_DEVICE_EXTENSION;

typedef struct _ding_recorder {
    INT32 number;
    INT32 x[0x10];
} DING_RECORDER, *PDING_RECORDER;

typedef struct _ding_msg {
    INT32 state;
    DING_RECORDER recorder;
} DING_MSG, *PDING_MSG;

// {4a8374da-c2db-4895-b7a0-8deed89b4643}
static const GUID GUID_HookMessageBoxAInterface = {0x4A8374DA, 0xc2db, 0x4895,
                                                   {0xb7, 0xa0, 0x8d, 0xee, 0xd8, 0x9b, 0x46, 0x43}};

PDEVICE_OBJECT g_pDevObj = NULL; // 自定义设备，用于和3环通信
DING_RECORDER g_Recorder; // 定义一个全局的recorder，在3环300ms轮询一次

#ifdef __cplusplus
extern "C" NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath);
#endif

VOID __stdcall realHooker(INT32 ESP3) {

    DING_RECORDER recorder;
    recorder.number = 4;
    recorder.x[0] = ((PINT32) ESP3)[1];
    recorder.x[1] = ((PINT32) ESP3)[2];
    recorder.x[2] = ((PINT32) ESP3)[3];
    recorder.x[3] = ((PINT32) ESP3)[4];
    g_Recorder = recorder;
}

VOID msgHooker() {

    _asm{
            pushad;
            pushfd;
            mov eax,[esp + 0x24 + 0x0C];
            push eax;
            call realHooker;
            popfd;
            popad;
            iretd;
    }
}

// 构造提权中断门，返回中断号
USHORT SetIntGate(UINT32 pFuncion) {
    UCHAR IDT[6]; // IDT寄存器
    UINT32 IdtAddr, IdtLen;
    UINT32 IntGateHi = 0, IntGateLo = 0; // 中断门描述符
    UINT32 *pPreIntGateAddr = (UINT32 *) g_pDevObj->DeviceExtension + 1;
    UINT32 i;
    // 构造中断门描述符
    IntGateLo = ((pFuncion & 0x0000FFFF) | 0x00080000);
    IntGateHi = ((pFuncion & 0xFFFF0000) | 0x0000EE00);
    // 遍历IDT，找无效项
    _asm{
            sidt fword ptr IDT;
    }
    IdtAddr = *(PULONG) (IDT + 2);
    IdtLen = *(PUSHORT) IDT;
    // 遍历IDT，找一个P=0的（跳过第一项）
    if ((*pPreIntGateAddr) == 0) {
        for (i = 8; i < IdtLen; i += 8) {
            if ((((PUINT32) (IdtAddr + i))[1] & 0x00008000) == 0) {
                // P=0，此处GDT表项无效，可以使用
                ((PUINT32) (IdtAddr + i))[0] = IntGateLo;
                ((PUINT32) (IdtAddr + i))[1] = IntGateHi;
                (*pPreIntGateAddr) = IdtAddr + i;
                break;
            }
        }
    } else {
        ((PUINT32) (*pPreIntGateAddr))[0] = IntGateLo;
        ((PUINT32) (*pPreIntGateAddr))[1] = IntGateHi;
    }

    //DbgPrint("*pPreIntGateAddr: %p.\n", *pPreIntGateAddr);
    //DbgPrint("INT %02X\n", (USHORT)((*pPreIntGateAddr - IdtAddr) / 8));
    if (*pPreIntGateAddr == 0) return 0;
    return (USHORT) ((*pPreIntGateAddr - IdtAddr) / 8);
}

// 构造提权调用门，返回调用门选择子
USHORT SetCallGate(UINT32 pFunction, UINT32 nParam) {
    UINT32 CallGateHi = 0, CallGateLo = 0; // 调用门描述符
    UCHAR GDT[6]; // GDT寄存器
    UINT32 GdtAddr, GdtLen;
    UINT32 i;
    UINT32 *pPreCallGateAddr = (UINT32 *) g_pDevObj->DeviceExtension;

    // 构造调用门
    CallGateHi = (pFunction & 0xFFFF0000);
    CallGateHi |= 0x0000EC00;
    CallGateHi |= nParam;
    CallGateLo = (pFunction & 0x0000FFFF);
    CallGateLo |= 0x00080000;
    // 获取GDT基址和大小
    _asm
    {
        sgdt fword ptr GDT;
    }
    GdtAddr = *(PULONG) (GDT + 2);
    GdtLen = *(PUSHORT) GDT;
    // 遍历GDT，找一个P=0的（跳过第一项）
    if ((*pPreCallGateAddr) == 0) {
        for (i = 8; i < GdtLen; i += 8) {
            //DbgPrint("%p\n",(PUINT32)(GdtAddr + i));
            if ((((PUINT32) (GdtAddr + i))[1] & 0x00008000) == 0) {
                // P=0，此处GDT表项无效，可以使用
                ((PUINT32) (GdtAddr + i))[0] = CallGateLo;
                ((PUINT32) (GdtAddr + i))[1] = CallGateHi;
                (*pPreCallGateAddr) = GdtAddr + i;
                break;
            }
        }
    } else {
        ((PUINT32) (*pPreCallGateAddr))[0] = CallGateLo;
        ((PUINT32) (*pPreCallGateAddr))[1] = CallGateHi;
    }
    if (*pPreCallGateAddr == 0) return 0;
    return (USHORT) ((*pPreCallGateAddr) - GdtAddr);
}

NTSTATUS IrpDeviceContrlProc(PDEVICE_OBJECT DeviceObject, PIRP pIrp) {

    NTSTATUS Status = STATUS_INVALID_DEVICE_REQUEST;
    PIO_STACK_LOCATION pIrpStack;//定义一个指向IO_STACK_LOCATION结构体的指针
    ULONG uIoControCode;
    PVOID pIoBuffer;
    ULONG uInLength;
    ULONG uOutLength;
    ULONG uRead;

    //获取缓冲区地址(输入和输出的缓冲区都是一个)
    pIoBuffer = pIrp->AssociatedIrp.SystemBuffer;

    //从当前Irp中获取数据
    pIrpStack = IoGetCurrentIrpStackLocation(pIrp);//根据从ring3发来的

    //获取控制码  Parameters里面是一个联合体  Read Write DeviceIoControl
    uIoControCode = pIrpStack->Parameters.DeviceIoControl.IoControlCode;

    //ring 3发送数据的长度
    uInLength = pIrpStack->Parameters.DeviceIoControl.InputBufferLength;

    //ring 0 发送数据的长度
    uOutLength = pIrpStack->Parameters.DeviceIoControl.OutputBufferLength;


    switch (uIoControCode) {
        case OPER1: {
            DbgPrint("IrpDeviceContrlProc -> OPER1 ...\n");
            pIrp->IoStatus.Information = 0;
            break;
        }
        case OPER2: {
            DbgPrint("IrpDeviceContrlProc -> OPER2 接受字节数:%x  \n", uInLength);
            DbgPrint("IrpDeviceContrlProc -> OPER2 返回字节数:%x  \n", uOutLength);

            //读取3环传过来的数据
            RtlCopyMemory(&uRead, pIoBuffer, 4);
            DbgPrint("IrpDeviceContrlProc -> OPER2 ...%x  \n", uRead);

            //把数据传回给3环
            *(ULONG *) pIoBuffer = 0x1314520;

            //set Status
            //设置给3环返回数据的字节数
            pIrp->IoStatus.Information = 4;

            break;
        }
        case OPER_SETINT: {
            USHORT INTNumber = SetIntGate((UINT32) msgHooker);
            DbgPrint("设置中断号:%x  \n", INTNumber);
            *(USHORT *) pIoBuffer = INTNumber;
            pIrp->IoStatus.Information = sizeof(USHORT);
            break;
        }
    }

    //设置返回状态
    DbgPrint("DispatchDeviceControl ...  \n");
    pIrp->IoStatus.Status = STATUS_SUCCESS;
    IoCompleteRequest(pIrp, IO_NO_INCREMENT);
    return STATUS_SUCCESS;
}

NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath) {

    g_Recorder.number = 0;

    DriverObject->MajorFunction[IRP_MJ_CREATE] = HookMessageBoxACreateClose;
    DriverObject->MajorFunction[IRP_MJ_CLOSE] = HookMessageBoxACreateClose;
    DriverObject->MajorFunction[IRP_MJ_PNP] = HookMessageBoxAPnP;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = IrpDeviceContrlProc;

    DriverObject->DriverUnload = HookMessageBoxAUnload;
    DriverObject->DriverStartIo = NULL;
    DriverObject->DriverExtension->AddDevice = HookMessageBoxAAddDevice;

    return STATUS_SUCCESS;
}

void HookMessageBoxAUnload(IN PDRIVER_OBJECT DriverObject) {
    DbgPrint("Goodbye from HookMessageBoxA!\n");
}

NTSTATUS HookMessageBoxACreateClose(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return STATUS_SUCCESS;
}

NTSTATUS HookMessageBoxADefaultHandler(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
    PHookMessageBoxA_DEVICE_EXTENSION deviceExtension = NULL;

    IoSkipCurrentIrpStackLocation(Irp);
    deviceExtension = (PHookMessageBoxA_DEVICE_EXTENSION) DeviceObject->DeviceExtension;
    return IoCallDriver(deviceExtension->TargetDeviceObject, Irp);
}

NTSTATUS HookMessageBoxAAddDevice(IN PDRIVER_OBJECT DriverObject, IN PDEVICE_OBJECT PhysicalDeviceObject) {
    PDEVICE_OBJECT DeviceObject = NULL;
    PHookMessageBoxA_DEVICE_EXTENSION pExtension = NULL;
    NTSTATUS status;

    status = IoCreateDevice(DriverObject,
                            sizeof(HookMessageBoxA_DEVICE_EXTENSION),
                            NULL,
                            FILE_DEVICE_UNKNOWN,
                            0,
                            0,
                            &DeviceObject);

    if (!NT_SUCCESS(status))
        return status;

    pExtension = (PHookMessageBoxA_DEVICE_EXTENSION) DeviceObject->DeviceExtension;

    pExtension->DeviceObject = DeviceObject;
    pExtension->PhysicalDeviceObject = PhysicalDeviceObject;
    pExtension->TargetDeviceObject = IoAttachDeviceToDeviceStack(DeviceObject, PhysicalDeviceObject);

    status = IoRegisterDeviceInterface(PhysicalDeviceObject, &GUID_HookMessageBoxAInterface, NULL,
                                       &pExtension->DeviceInterface);
    ASSERT(NT_SUCCESS(status));

    DeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
    return STATUS_SUCCESS;
}


NTSTATUS HookMessageBoxAIrpCompletion(
        IN PDEVICE_OBJECT DeviceObject,
        IN PIRP Irp,
        IN PVOID Context
) {
    PKEVENT Event = (PKEVENT) Context;

    UNREFERENCED_PARAMETER(DeviceObject);
    UNREFERENCED_PARAMETER(Irp);

    KeSetEvent(Event, IO_NO_INCREMENT, FALSE);

    return (STATUS_MORE_PROCESSING_REQUIRED);
}

NTSTATUS HookMessageBoxAForwardIrpSynchronous(
        IN PDEVICE_OBJECT DeviceObject,
        IN PIRP Irp
) {
    PHookMessageBoxA_DEVICE_EXTENSION deviceExtension;
    KEVENT event;
    NTSTATUS status;

    KeInitializeEvent(&event, NotificationEvent, FALSE);
    deviceExtension = (PHookMessageBoxA_DEVICE_EXTENSION) DeviceObject->DeviceExtension;

    IoCopyCurrentIrpStackLocationToNext(Irp);

    IoSetCompletionRoutine(Irp, HookMessageBoxAIrpCompletion, &event, TRUE, TRUE, TRUE);

    status = IoCallDriver(deviceExtension->TargetDeviceObject, Irp);

    if (status == STATUS_PENDING) {
        KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
        status = Irp->IoStatus.Status;
    }
    return status;
}

NTSTATUS HookMessageBoxAPnP(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp) {
    PIO_STACK_LOCATION irpSp = IoGetCurrentIrpStackLocation(Irp);
    PHookMessageBoxA_DEVICE_EXTENSION pExt = ((PHookMessageBoxA_DEVICE_EXTENSION) DeviceObject->DeviceExtension);
    NTSTATUS status;

    ASSERT(pExt);

    switch (irpSp->MinorFunction) {
        case IRP_MN_START_DEVICE:
            IoSetDeviceInterfaceState(&pExt->DeviceInterface, TRUE);
            Irp->IoStatus.Status = STATUS_SUCCESS;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            return STATUS_SUCCESS;

        case IRP_MN_QUERY_REMOVE_DEVICE:
            Irp->IoStatus.Status = STATUS_SUCCESS;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            return STATUS_SUCCESS;

        case IRP_MN_REMOVE_DEVICE:
            IoSetDeviceInterfaceState(&pExt->DeviceInterface, FALSE);
            status = HookMessageBoxAForwardIrpSynchronous(DeviceObject, Irp);
            IoDetachDevice(pExt->TargetDeviceObject);
            IoDeleteDevice(pExt->DeviceObject);
            RtlFreeUnicodeString(&pExt->DeviceInterface);
            Irp->IoStatus.Status = STATUS_SUCCESS;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            return STATUS_SUCCESS;

        case IRP_MN_QUERY_PNP_DEVICE_STATE:
            status = HookMessageBoxAForwardIrpSynchronous(DeviceObject, Irp);
            Irp->IoStatus.Information = 0;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            return status;
    }
    return HookMessageBoxADefaultHandler(DeviceObject, Irp);
}
