package table
import (
"github.com/go-jet/jet/v2/postgres"
)
var Users = newUsersTable("public", "users", "")
type usersTable struct {
postgres.Table
ID         postgres.ColumnString
Username   postgres.ColumnString
LastOnline postgres.ColumnTimestamp
CreatedAt  postgres.ColumnTimestamp
AllColumns     postgres.ColumnList
MutableColumns postgres.ColumnList
}
type UsersTable struct {
usersTable
EXCLUDED usersTable
}
func (a UsersTable) AS(alias string) *UsersTable {
return newUsersTable(a.SchemaName(), a.TableName(), alias)
}
func (a UsersTable) FromSchema(schemaName string) *UsersTable {
return newUsersTable(schemaName, a.TableName(), a.Alias())
}
func (a UsersTable) WithPrefix(prefix string) *UsersTable {
return newUsersTable(a.SchemaName(), prefix+a.TableName(), a.TableName())
}
func (a UsersTable) WithSuffix(suffix string) *UsersTable {
return newUsersTable(a.SchemaName(), a.TableName()+suffix, a.TableName())
}
func newUsersTable(schemaName, tableName, alias string) *UsersTable {
return &UsersTable{
usersTable: newUsersTableImpl(schemaName, tableName, alias),
EXCLUDED:   newUsersTableImpl("", "excluded", ""),
}
}
func newUsersTableImpl(schemaName, tableName, alias string) usersTable {
var (
IDColumn         = postgres.StringColumn("id")
UsernameColumn   = postgres.StringColumn("username")
LastOnlineColumn = postgres.TimestampColumn("last_online")
CreatedAtColumn  = postgres.TimestampColumn("created_at")
allColumns       = postgres.ColumnList{IDColumn, UsernameColumn, LastOnlineColumn, CreatedAtColumn}
mutableColumns   = postgres.ColumnList{UsernameColumn, LastOnlineColumn, CreatedAtColumn}
)
return usersTable{
Table: postgres.NewTable(schemaName, tableName, alias, allColumns...),
ID:         IDColumn,
Username:   UsernameColumn,
LastOnline: LastOnlineColumn,
CreatedAt:  CreatedAtColumn,
AllColumns:     allColumns,
MutableColumns: mutableColumns,
}
}