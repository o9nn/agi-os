package model
import (
	"github.com/google/uuid"
	"time"
)
type Messages struct {
	ID        uuid.UUID `sql:"primary_key"`
	ChatID    uuid.UUID
	Sender    ChatUser
	Content   string
	CreatedAt time.Time
}