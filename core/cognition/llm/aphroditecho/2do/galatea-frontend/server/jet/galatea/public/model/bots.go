package model
import (
"github.com/google/uuid"
"time"
)
type Bots struct {
ID          uuid.UUID `sql:"primary_key"`
OwnerID     uuid.UUID
Name        string
Description string
AvatarURL   string
Personality string
CreatedAt   time.Time
}