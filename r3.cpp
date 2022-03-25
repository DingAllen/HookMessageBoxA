#include<Windows.h>
#include<winioctl.h>
#include<stdio.h>

#define IN_BUFFER_MAXLENGTH  0x100
#define OUT_BUFFER_MAXLENGTH  0x100
//宏定义之获取一个32位的宏控制码  参数:设备类型(鼠标,键盘...Unkonwn);0x000-0x7FF保留,0x800-0xfff随便填一个;数据交互类型(缓冲区,IO,其他);对这个设备的权限
#define OPER1 CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define OPER2 CTL_CODE(FILE_DEVICE_UNKNOWN,0x900,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define OPER_SETINT CTL_CODE(FILE_DEVICE_UNKNOWN,0x845,METHOD_BUFFERED,FILE_ANY_ACCESS)
#define OPER_RETURNREC CTL_CODE(FILE_DEVICE_UNKNOWN,0x945,METHOD_BUFFERED,FILE_ANY_ACCESS)

#define SYMBOLICLINK_NAME  "\\\\.\\MsgHooker"

typedef struct _ding_recorder {
    INT32 number;
    INT32 x[0x10];
} DING_RECORDER, *PDING_RECORDER;

HANDLE g_hDevice;  //全局驱动句柄

//打开驱动服务句柄
//3环链接名:\\\\.\\AABB

BOOL Open(PCHAR pLinkName) {
    //在3环获取设备句柄
    TCHAR szBuffer[10] = {0};
    g_hDevice = ::CreateFile(pLinkName, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    DWORD err = ::GetLastError();
    sprintf(szBuffer, "%d\n", err);
    if (g_hDevice != INVALID_HANDLE_VALUE)
        return TRUE;
    else
        return FALSE;

}

BOOL IoControl(DWORD dwIoCode, PVOID InBuff, DWORD InBuffLen, PVOID OutBuff, DWORD OutBuffLen) {
    DWORD dw;
    //设备句柄/控制码/输入缓冲区地址/输入缓冲区长度/输出缓冲区地址/输出缓冲区长度/返回长度/指向OVERLAPPED 此处为空
    ::DeviceIoControl(g_hDevice, dwIoCode, InBuff, InBuffLen, OutBuff, OutBuffLen, &dw, NULL);
    //这一串东西最后会转成IRP发生给设备,设备再从IRP中分解出数据
    return TRUE;
}

int main(int argc, char *argv[]) {

    DWORD dwInBuffer = (DWORD) MessageBoxA;
    TCHAR szOutBuffer[OUT_BUFFER_MAXLENGTH] = {0};

    __asm
    {
        mov
        eax, dword
        ptr
        ds:[MessageBoxA];
        mov
        eax,[eax];
    }

    // 通过符号链接,打开设备
    Open(SYMBOLICLINK_NAME);

    // 过写保护，设置中断门，完成Hook
    IoControl(OPER_SETINT, &dwInBuffer, IN_BUFFER_MAXLENGTH, szOutBuffer, OUT_BUFFER_MAXLENGTH);
    *(PUCHAR) MessageBoxA = 0xCD;
    *((PUCHAR) MessageBoxA + 1) = *(PUCHAR) szOutBuffer;

    // 轮询，接收Hook到的MessageBoxA
    while (true) {
        IoControl(OPER_RETURNREC, &dwInBuffer, IN_BUFFER_MAXLENGTH, szOutBuffer, OUT_BUFFER_MAXLENGTH);
        DING_RECORDER rec = *(PDING_RECORDER) szOutBuffer;
        if (rec.number != 0) {
            printf("Hook到一个MessageBoxA，参数有%d个 \n", rec.number);
            for (int i = 0; i < rec.number; ++i) {
                printf("参数%d：%08lX \n", i, rec.x[i]);
            }
            printf("\n");
        }
        Sleep(300);
    }

    // 关闭设备
    CloseHandle(g_hDevice);
    getchar();
    return 0;
}
