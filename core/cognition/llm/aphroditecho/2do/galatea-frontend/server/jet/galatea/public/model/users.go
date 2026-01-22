package model
import (
	"github.com/google/uuid"
	"time"
)
type Users struct {
	ID         uuid.UUID `sql:"primary_key"`
	Username   string
	LastOnline time.Time
	CreatedAt  time.Time
}