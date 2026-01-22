package enum
import "github.com/go-jet/jet/v2/postgres"
var ChatUser = &struct {
User postgres.StringExpression
Bot  postgres.StringExpression
}{
User: postgres.NewEnumValue("user"),
Bot:  postgres.NewEnumValue("bot"),
}