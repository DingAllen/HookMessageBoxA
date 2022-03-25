#include "stdafx.h"

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

// {4a8374da-c2db-4895-b7a0-8deed89b4643}
static const GUID GUID_HookMessageBoxAInterface = {0x4A8374DA, 0xc2db, 0x4895,
                                                   {0xb7, 0xa0, 0x8d, 0xee, 0xd8, 0x9b, 0x46, 0x43}};

PDEVICE_OBJECT g_pDevObj = NULL; // �Զ����豸�����ں�3��ͨ��

#ifdef __cplusplus
extern "C" NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath);
#endif

// ������Ȩ�ж��ţ������жϺ�
USHORT SetIntGate(UINT32 pFuncion) {
    UCHAR IDT[6]; // IDT�Ĵ���
    UINT32 IdtAddr, IdtLen;
    UINT32 IntGateHi = 0, IntGateLo = 0; // �ж���������
    UINT32 *pPreIntGateAddr = (UINT32 *) g_pDevObj->DeviceExtension + 1;
    UINT32 i;
    // �����ж���������
    IntGateLo = ((pFuncion & 0x0000FFFF) | 0x00080000);
    IntGateHi = ((pFuncion & 0xFFFF0000) | 0x0000EE00);
    // ����IDT������Ч��
    _asm{
            sidt fword ptr IDT;
    }
    IdtAddr = *(PULONG) (IDT + 2);
    IdtLen = *(PUSHORT) IDT;
    // ����IDT����һ��P=0�ģ�������һ�
    if ((*pPreIntGateAddr) == 0) {
        for (i = 8; i < IdtLen; i += 8) {
            if ((((PUINT32) (IdtAddr + i))[1] & 0x00008000) == 0) {
                // P=0���˴�GDT������Ч������ʹ��
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

// ������Ȩ�����ţ����ص�����ѡ����
USHORT SetCallGate(UINT32 pFunction, UINT32 nParam) {
    UINT32 CallGateHi = 0, CallGateLo = 0; // ������������
    UCHAR GDT[6]; // GDT�Ĵ���
    UINT32 GdtAddr, GdtLen;
    UINT32 i;
    UINT32 *pPreCallGateAddr = (UINT32 *) g_pDevObj->DeviceExtension;

    // ���������
    CallGateHi = (pFunction & 0xFFFF0000);
    CallGateHi |= 0x0000EC00;
    CallGateHi |= nParam;
    CallGateLo = (pFunction & 0x0000FFFF);
    CallGateLo |= 0x00080000;
    // ��ȡGDT��ַ�ʹ�С
    _asm
    {
        sgdt
        fword
        ptr GDT;
    }
    GdtAddr = *(PULONG) (GDT + 2);
    GdtLen = *(PUSHORT) GDT;
    // ����GDT����һ��P=0�ģ�������һ�
    if ((*pPreCallGateAddr) == 0) {
        for (i = 8; i < GdtLen; i += 8) {
            //DbgPrint("%p\n",(PUINT32)(GdtAddr + i));
            if ((((PUINT32) (GdtAddr + i))[1] & 0x00008000) == 0) {
                // P=0���˴�GDT������Ч������ʹ��
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

NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath) {
    unsigned i;

    DbgPrint("Hello from HookMessageBoxA!\n");

    for (i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++)
        DriverObject->MajorFunction[i] = HookMessageBoxADefaultHandler;

    DriverObject->MajorFunction[IRP_MJ_CREATE] = HookMessageBoxACreateClose;
    DriverObject->MajorFunction[IRP_MJ_CLOSE] = HookMessageBoxACreateClose;
    DriverObject->MajorFunction[IRP_MJ_PNP] = HookMessageBoxAPnP;

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
