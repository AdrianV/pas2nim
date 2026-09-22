{.feature: "lenientnils".}
# Delphi Windows-unit compat surface for the corpus's MSWINDOWS
# branches (SyncObjs first: the sync primitives and the CoWait thread
# machinery). Stub-only on Linux: the sync objects get opaque dummy
# handles and the API calls return the Delphi-shaped success forms, so
# the single-threaded front-end exercises the same code paths.
import systempas

type
  THandle* = uint32      # Delphi 2007 (Win32): THandle = LongWord
  DWORD* = uint32
  HRESULT* = int32       # Delphi: HRESULT = Longint
  HWND* = uint32         # D2007: HWND = type LongWord
  HWnd* = uint32         # the D2007 spelling (nimony is case-sensitive)
  PSecurityAttributes* = pointer
  PHANDLE* = ptr THandle
  HMODULE* = uint32     # D2007: HMODULE = HINST = THandle
  HINSTANCE* = uint32
  # the exact TCoWait signature (Libc.nim's M10 lesson): the corpus
  # assigns GetProcAddress's result to its own proc-type var
  FARPROC* = proc (dwFlags, dwTimeOut, cHandles: uint32;
                   Handles: var pointer; lpdwIndex: var uint32): int32 {.stdcall.}

  TRTLCriticalSection* = object
    LockCount*: int32
    RecursionCount*: int32
    OwningThread*: uint32
    LockSemaphore*: uint32
    SpinCount*: uint32

  TMsg* = object
    hwnd*: HWND
    message*: uint32
    wParam*: int64
    lParam*: int64
    time*: DWORD
    pt*: TPoint

const
  VER_PLATFORM_WIN32s* = 0'i32
  VER_PLATFORM_WIN32_WINDOWS* = 1'i32
  VER_PLATFORM_WIN32_NT* = 2'i32

var
  Win32Platform* = 2'i32     # VER_PLATFORM_WIN32_NT (v1 constant)
  Win32MajorVersion* = 5'i32
  Win32MinorVersion* = 0'i32

const
  HWND_MESSAGE* = 0xFFFFFFFD'u32   # HWND(-3) in D2007's unsigned handles
  WAIT_OBJECT_0* = 0'u32
  WAIT_ABANDONED_0* = 128'u32
  WAIT_ABANDONED* = 128'u32   # WAIT_ABANDONED_0 + 0x80
  WAIT_TIMEOUT* = 0x00000102'u32
  WAIT_IO_COMPLETION* = 0x000000C0'u32
  WAIT_FAILED* = 0xFFFFFFFF'u32
  INFINITE* = 0xFFFFFFFF'u32
  QS_ALLEVENTS* = 0x000004BF'u32
  PM_REMOVE* = 1'u32
  RPC_E_TIMEOUT* = int32(-2147417826)
  RPC_S_CALLPENDING* = int32(-2147417835)
  S_OK* = 0'i32

# Win32 sync-object creation: the stubs hand out distinct nonzero
# handles (handle 0 is WAIT_FAILED-shaped, never returned here)
var pasWinHandleCounter* = 2'u32

proc NextPasHandle*(): THandle =
  result = pasWinHandleCounter
  pasWinHandleCounter = pasWinHandleCounter + 1

proc CreateMutex*(MutexAttributes: PSecurityAttributes;
                  InitialOwner: bool; Name: cstring): THandle =
  result = NextPasHandle()

proc CreateEvent*(EventAttributes: PSecurityAttributes;
                  ManualReset: bool; InitialState: bool;
                  Name: cstring): THandle =
  result = NextPasHandle()

proc CloseHandle*(hObject: THandle): bool =
  result = true

proc ReleaseMutex*(hMutex: THandle): bool =
  result = true

proc WaitForSingleObject*(hHandle: THandle; dwMilliseconds: DWORD): DWORD =
  result = WAIT_OBJECT_0  # the object is always signaled

proc WaitForMultipleObjectsEx*(nCount: DWORD; Handles: var pointer;
                               bWaitAll: bool; dwMilliseconds: DWORD;
                               bAlertable: bool): DWORD =
  result = WAIT_OBJECT_0

proc MsgWaitForMultipleObjectsEx*(nCount: DWORD; Handles: var pointer;
                                  dwMilliseconds: DWORD;
                                  dwWakeMask: DWORD;
                                  dwFlags: DWORD): DWORD =
  result = WAIT_OBJECT_0

proc GetLastError*(): DWORD =
  result = 0'u32

proc GetCurrentThreadId*(): DWORD =
  result = 0'u32

proc SetEvent*(hEvent: THandle): bool =
  result = true

proc ResetEvent*(hEvent: THandle): bool =
  result = true

# critical sections: stubs never block
proc InitializeCriticalSection*(Section: var TRTLCriticalSection) =
  Section = default(TRTLCriticalSection)

proc DeleteCriticalSection*(Section: var TRTLCriticalSection) =
  discard

proc EnterCriticalSection*(Section: var TRTLCriticalSection) =
  discard

proc LeaveCriticalSection*(Section: var TRTLCriticalSection) =
  discard

proc TryEnterCriticalSection*(Section: var TRTLCriticalSection): bool =
  result = true

# the CoWait fallback needs the ole-thread window lookups
proc IsWindow*(hWnd: HWND): bool =
  result = false

proc FindWindowEx*(hParent: HWND; hChild: HWND; className: string;
                   title: cstring): HWND =
  result = 0'u32

proc GetWindowThreadProcessId*(hWnd: HWND; pid: RootRef): DWORD =
  result = 0'u32

proc PeekMessage*(msg: var TMsg; hWnd: HWND; min: uint32; max: uint32;
                  remove: uint32): bool =
  result = false

proc TranslateMessage*(msg: var TMsg): bool =
  result = true

proc DispatchMessage*(msg: var TMsg): int64 =
  result = 0

proc GetModuleHandle*(name: cstring): HMODULE =
  result = 0'u32

proc GetProcAddress*(module: HMODULE; name: cstring): FARPROC =
  result = default(FARPROC)