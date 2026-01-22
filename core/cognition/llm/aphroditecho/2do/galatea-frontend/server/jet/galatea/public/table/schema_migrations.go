package table
import (
"github.com/go-jet/jet/v2/postgres"
)
var SchemaMigrations = newSchemaMigrationsTable("public", "schema_migrations", "")
type schemaMigrationsTable struct {
postgres.Table
Version postgres.ColumnInteger
Dirty   postgres.ColumnBool
AllColumns     postgres.ColumnList
MutableColumns postgres.ColumnList
}
type SchemaMigrationsTable struct {
schemaMigrationsTable
EXCLUDED schemaMigrationsTable
}
func (a SchemaMigrationsTable) AS(alias string) *SchemaMigrationsTable {
return newSchemaMigrationsTable(a.SchemaName(), a.TableName(), alias)
}
func (a SchemaMigrationsTable) FromSchema(schemaName string) *SchemaMigrationsTable {
return newSchemaMigrationsTable(schemaName, a.TableName(), a.Alias())
}
func (a SchemaMigrationsTable) WithPrefix(prefix string) *SchemaMigrationsTable {
return newSchemaMigrationsTable(a.SchemaName(), prefix+a.TableName(), a.TableName())
}
func (a SchemaMigrationsTable) WithSuffix(suffix string) *SchemaMigrationsTable {
return newSchemaMigrationsTable(a.SchemaName(), a.TableName()+suffix, a.TableName())
}
func newSchemaMigrationsTable(schemaName, tableName, alias string) *SchemaMigrationsTable {
return &SchemaMigrationsTable{
schemaMigrationsTable: newSchemaMigrationsTableImpl(schemaName, tableName, alias),
EXCLUDED:              newSchemaMigrationsTableImpl("", "excluded", ""),
}
}
func newSchemaMigrationsTableImpl(schemaName, tableName, alias string) schemaMigrationsTable {
var (
VersionColumn  = postgres.IntegerColumn("version")
DirtyColumn    = postgres.BoolColumn("dirty")
allColumns     = postgres.ColumnList{VersionColumn, DirtyColumn}
mutableColumns = postgres.ColumnList{DirtyColumn}
)
return schemaMigrationsTable{
Table: postgres.NewTable(schemaName, tableName, alias, allColumns...),
Version: VersionColumn,
Dirty:   DirtyColumn,
AllColumns:     allColumns,
MutableColumns: mutableColumns,
}
}