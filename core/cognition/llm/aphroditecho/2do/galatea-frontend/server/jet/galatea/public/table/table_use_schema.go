package table
func UseSchema(schema string) {
Bots = Bots.FromSchema(schema)
Chats = Chats.FromSchema(schema)
Messages = Messages.FromSchema(schema)
SchemaMigrations = SchemaMigrations.FromSchema(schema)
Users = Users.FromSchema(schema)
}