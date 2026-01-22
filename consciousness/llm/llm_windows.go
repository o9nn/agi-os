package llm
import (
	"syscall"
)
const (
	CREATE_DEFAULT_ERROR_MODE   = 0x04000000
	ABOVE_NORMAL_PRIORITY_CLASS = 0x00008000
	CREATE_NO_WINDOW            = 0x08000000
)
var LlamaServerSysProcAttr = &syscall.SysProcAttr{
	CreationFlags: CREATE_DEFAULT_ERROR_MODE | ABOVE_NORMAL_PRIORITY_CLASS | CREATE_NO_WINDOW,
}