package v1
import (
"embed"
"github.com/samber/lo"
)
var openAPIV3SpecJSON embed.FS
var openAPIV3SpecYaml embed.FS
func OpenAPIV3SpecJSON() []byte {
return lo.Must(openAPIV3SpecJSON.ReadFile("v1.swagger.v3.json"))
}
func OpenAPIV3SpecYaml() []byte {
return lo.Must(openAPIV3SpecYaml.ReadFile("v1.swagger.v3.yaml"))
}