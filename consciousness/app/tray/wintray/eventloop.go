package wintray
import (
	"fmt"
	"log/slog"
	"sync"
	"unsafe"
	"golang.org/x/sys/windows"
)
var quitOnce sync.Once
func (t *winTray) Run() {
	nativeLoop()
}
func nativeLoop() {
	slog.Debug("starting event handling loop")
	m := &struct {
		WindowHandle windows.Handle
		Message      uint32
		Wparam       uintptr
		Lparam       uintptr
		Time         uint32
		Pt           point
		LPrivate     uint32
	}{}
	for {
		ret, _, err := pGetMessage.Call(uintptr(unsafe.Pointer(m)), 0, 0, 0)
		switch int32(ret) {
		case -1:
			slog.Error(fmt.Sprintf("get message failure: %v", err))
			return
		case 0:
			return
		default:
			pTranslateMessage.Call(uintptr(unsafe.Pointer(m))) 
			pDispatchMessage.Call(uintptr(unsafe.Pointer(m)))  
		}
	}
}
func (t *winTray) wndProc(hWnd windows.Handle, message uint32, wParam, lParam uintptr) (lResult uintptr) {
	const (
		WM_RBUTTONUP   = 0x0205
		WM_LBUTTONUP   = 0x0202
		WM_COMMAND     = 0x0111
		WM_ENDSESSION  = 0x0016
		WM_CLOSE       = 0x0010
		WM_DESTROY     = 0x0002
		WM_MOUSEMOVE   = 0x0200
		WM_LBUTTONDOWN = 0x0201
	)
	switch message {
	case WM_COMMAND:
		menuItemId := int32(wParam)
		switch menuItemId {
		case quitMenuID:
			select {
			case t.callbacks.Quit <- struct{}{}:
			default:
				slog.Error("no listener on Quit")
			}
		case updateMenuID:
			select {
			case t.callbacks.Update <- struct{}{}:
			default:
				slog.Error("no listener on Update")
			}
		case diagLogsMenuID:
			select {
			case t.callbacks.ShowLogs <- struct{}{}:
			default:
				slog.Error("no listener on ShowLogs")
			}
		default:
			slog.Debug(fmt.Sprintf("Unexpected menu item id: %d", menuItemId))
		}
	case WM_CLOSE:
		boolRet, _, err := pDestroyWindow.Call(uintptr(t.window))
		if boolRet == 0 {
			slog.Error(fmt.Sprintf("failed to destroy window: %s", err))
		}
		err = t.wcex.unregister()
		if err != nil {
			slog.Error(fmt.Sprintf("failed to unregister window %s", err))
		}
	case WM_DESTROY:
		defer pPostQuitMessage.Call(uintptr(int32(0))) 
		fallthrough
	case WM_ENDSESSION:
		t.muNID.Lock()
		if t.nid != nil {
			err := t.nid.delete()
			if err != nil {
				slog.Error(fmt.Sprintf("failed to delete nid: %s", err))
			}
		}
		t.muNID.Unlock()
	case t.wmSystrayMessage:
		switch lParam {
		case WM_MOUSEMOVE, WM_LBUTTONDOWN:
		case WM_RBUTTONUP, WM_LBUTTONUP:
			err := t.showMenu()
			if err != nil {
				slog.Error(fmt.Sprintf("failed to show menu: %s", err))
			}
		case 0x405: 
			if t.pendingUpdate {
				select {
				case t.callbacks.Update <- struct{}{}:
				default:
					slog.Error("no listener on Update")
				}
			} else {
				select {
				case t.callbacks.DoFirstUse <- struct{}{}:
				default:
					slog.Error("no listener on DoFirstUse")
				}
			}
		case 0x404: 
		default:
			slog.Debug(fmt.Sprintf("unmanaged app message, lParm: 0x%x", lParam))
		}
	case t.wmTaskbarCreated: 
		t.muNID.Lock()
		err := t.nid.add()
		if err != nil {
			slog.Error(fmt.Sprintf("failed to refresh the taskbar on explorer restart: %s", err))
		}
		t.muNID.Unlock()
	default:
		lResult, _, _ = pDefWindowProc.Call(
			uintptr(hWnd),
			uintptr(message),
			wParam,
			lParam,
		)
	}
	return
}
func (t *winTray) Quit() {
	quitOnce.Do(quit)
}
func quit() {
	boolRet, _, err := pPostMessage.Call(
		uintptr(wt.window),
		WM_CLOSE,
		0,
		0,
	)
	if boolRet == 0 {
		slog.Error(fmt.Sprintf("failed to post close message on shutdown %s", err))
	}
}