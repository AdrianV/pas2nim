# Libc (Kylix) shim - only what the corpus's SyncObjs LINUX branch
# needs. The v1 stubs serialize nothing: TSemaphore and critical
# section calls compile but provide no cross-thread synchronization
# (documented divergence - the front-end is single-threaded).
import systempas

type
  TSemaphore* = ref object of RootRef
  TRTLCriticalSection* = ref object of RootRef

proc semInit*(s: TSemaphore; shared: bool; value: int32): int32 =
  result = 0

proc semWait*(s: TSemaphore): int32 =
  result = 0

proc semPost*(s: TSemaphore): int32 =
  result = 0

proc semGetValue*(s: TSemaphore; value: var int32): int32 =
  result = 0

proc semTryWait*(s: TSemaphore): int32 =
  result = 0

proc InitializeCriticalSection*(s: TRTLCriticalSection) = discard
proc DeleteCriticalSection*(s: TRTLCriticalSection) = discard
proc EnterCriticalSection*(s: TRTLCriticalSection) = discard
proc LeaveCriticalSection*(s: TRTLCriticalSection) = discard
proc TryEnterCriticalSection*(s: TRTLCriticalSection): bool =
  result = true
# Win32 compatibility surface used by SyncObjs's COM wait machinery
# (v1 stubs - the single-threaded front-end never opens a message
# window; documented divergence)
type
  HWND* = int64
  HWnd* = HWND
  HMODULE* = int64
  # the corpus's only GetProcAddress target: SyncObjs's
  # TCoWaitForMultipleHandlesProc - a matching proc type lets the
  # assignment type check (nimony has no pointer->proc cast)
  FARPROC* = proc (dwFlags: uint32; dwTimeOut: uint32; cHandles: uint32;
      Handles: var pointer; lpdwIndex: var uint32): int32 {.stdcall.}
  TMsg* = object
    hwnd*: HWND
    message*: int32
    wParam*: int
    lParam*: int

var Win32Platform*: int32 = 0
var Win32MajorVersion*: int32 = 0

const
  VER_PLATFORM_WIN32_NT* = 2
  HWND_MESSAGE* = -3
  QS_ALLEVENTS* = 0x000004BF'i32
  WAIT_OBJECT_0* = 0'u32
  WAIT_ABANDONED_0* = 0x00000080'u32
  WAIT_TIMEOUT* = 0x00000102'u32
  WAIT_IO_COMPLETION* = 0x000000C0'u32
  PM_REMOVE* = 1'i32
  RPC_E_TIMEOUT* = int32(-2147417846)
  RPC_S_CALLPENDING* = int32(-2147417835)
  S_OK* = 0'i32
  INFINITE* = 0xFFFFFFFF'u32

proc IsWindow*(h: HWND): bool =
  result = h != 0

proc FindWindowEx*(parent, child: HWND; cls: string; title: cstring): HWND =
  result = 0

proc GetWindowThreadProcessId*(h: HWND; pid: RootRef): int32 =
  result = 1

proc GetCurrentThreadId*(): int32 =
  result = 1

proc MsgWaitForMultipleObjectsEx*(nCount: uint32; handles: var pointer;
    dwMilliseconds: uint32; dwWakeMask: int32; dwFlags: uint32): uint32 =
  result = WAIT_TIMEOUT

proc PeekMessage*(msg: var TMsg; hWnd: HWND; min, max, remove: int32): bool =
  result = false

proc TranslateMessage*(msg: var TMsg): bool =
  result = false

proc DispatchMessage*(msg: var TMsg): int32 =
  result = 0

proc WaitForMultipleObjectsEx*(nCount: uint32; handles: pointer;
    bWaitAll: bool; dwMilliseconds: uint32; bAlertable: bool): uint32 =
  result = WAIT_TIMEOUT

proc GetModuleHandle*(name: string): HMODULE =
  result = 0

proc GetProcAddress*(m: HMODULE; name: string): FARPROC =
  # v1 stub: the proc-typed result defaults to nil; the caller's
  # Assigned() check then installs the internal fallback routine
  result = default(FARPROC)
