package model
import (
"github.com/google/uuid"
"time"
)
type Chats struct {
ID        uuid.UUID `sql:"primary_key"`
UserID    *uuid.UUID
BotID     *uuid.UUID
CreatedAt time.Time
}