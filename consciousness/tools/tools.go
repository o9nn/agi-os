package tools
import (
	"bytes"
	"encoding/json"
	"strings"
	"text/template"
	"github.com/EchoCog/echollama/api"
)
type toolsState int
const (
	toolsState_LookingForTag toolsState = iota
	toolsState_ToolCalling
	toolsState_Done
)
type Parser struct {
	tag   string
	tools []api.Tool
	state  toolsState
	buffer []byte
	n      int
}
func (p *Parser) GetBuffer() []byte {
	return p.buffer
}
func NewParser(tmpl *template.Template, tools []api.Tool) *Parser {
	return NewParserWithTag(tools, parseTag(tmpl))
}
func NewParserWithTag(tools []api.Tool, tag string) *Parser {
	return &Parser{
		tag:   tag,
		tools: tools,
	}
}
func (p *Parser) Add(s string) (calls []api.ToolCall, content string) {
	if p.state == toolsState_Done {
		return nil, s
	}
	p.buffer = append(p.buffer, s...)
	if p.state == toolsState_LookingForTag {
		i, found := p.findTag()
		if i == -1 {
			content = string(p.buffer)
			p.buffer = []byte{}
		} else {
			content = string(p.buffer[:i])
			p.buffer = p.buffer[i:]
		}
		if p.tag == "{" || p.tag == "[" {
			if strings.TrimSpace(content) != "" {
				p.state = toolsState_Done
				return nil, content + string(p.buffer)
			}
		}
		if !found {
			return nil, content
		}
		p.state = toolsState_ToolCalling
	}
	for {
		call := p.parseToolCall()
		if call == nil {
			break
		}
		calls = append(calls, *call)
	}
	if p.done() {
		p.state = toolsState_Done
		content = string(p.buffer)
		p.buffer = []byte{}
	}
	return calls, content
}
func (p *Parser) findTag() (int, bool) {
	if i := bytes.Index(p.buffer, []byte(p.tag)); i > -1 {
		return i, true
	}
	max := min(len(p.buffer), len(p.tag))
	for i := max; i > 0; i-- {
		if bytes.HasSuffix(p.buffer, []byte(p.tag[:i])) {
			return len(p.buffer) - i, false
		}
	}
	return -1, false
}
func (p *Parser) parseToolCall() *api.ToolCall {
	tool, end := findTool(p.tools, p.buffer)
	if tool == nil {
		return nil
	}
	var args map[string]any
	if found, i := findArguments(p.buffer); found == nil {
		return nil
	} else {
		args = found
		if i > end {
			end = i
		}
	}
	tc := &api.ToolCall{
		Function: api.ToolCallFunction{
			Name:      tool.Function.Name,
			Arguments: args,
			Index:     p.n,
		},
	}
	p.n++
	p.buffer = p.buffer[end:]
	return tc
}
func findTool(tools []api.Tool, buf []byte) (*api.Tool, int) {
	if len(buf) == 0 {
		return nil, 0
	}
	var longest string
	for _, t := range tools {
		if len(t.Function.Name) > len(longest) {
			longest = t.Function.Name
		}
	}
	for i := 1; i <= min(len(buf), len(longest)); i++ {
		tail := buf[len(buf)-i:]
		for _, t := range tools {
			name := []byte(t.Function.Name)
			if len(tail) < len(name) && bytes.HasPrefix(name, tail) {
				return nil, 0
			}
		}
	}
	var found *api.Tool
	start := -1
	end := -1
	for i := range tools {
		name := []byte(tools[i].Function.Name)
		pos := bytes.Index(buf, name)
		if pos == -1 {
			continue
		}
		if start != -1 {
			if pos > start {
				continue
			}
			if pos == start && len(name) <= len(found.Function.Name) {
				continue
			}
		}
		found = &tools[i]
		start = pos
		end = pos + len(name)
	}
	if found != nil {
		return found, end
	}
	return nil, 0
}
func findArguments(buffer []byte) (map[string]any, int) {
	if len(buffer) == 0 {
		return nil, 0
	}
	var braces int
	var start int = -1
	for i, c := range buffer {
		if c == '{' {
			if braces == 0 {
				start = i
			}
			braces++
		} else if c == '}' && braces > 0 {
			braces--
			if braces == 0 && start != -1 {
				object := buffer[start : i+1]
				var data map[string]any
				if err := json.Unmarshal(object, &data); err != nil {
					start = -1
					continue
				}
				var findObject func(obj map[string]any) (map[string]any, bool)
				findObject = func(obj map[string]any) (map[string]any, bool) {
					if _, hasName := obj["name"]; hasName {
						if args, ok := obj["arguments"].(map[string]any); ok {
							return args, true
						}
						if args, ok := obj["parameters"].(map[string]any); ok {
							return args, true
						}
						return nil, true
					}
					for _, v := range obj {
						switch child := v.(type) {
						case map[string]any:
							if result, found := findObject(child); found {
								return result, true
							}
						case []any:
							for _, item := range child {
								if childObj, ok := item.(map[string]any); ok {
									if result, found := findObject(childObj); found {
										return result, true
									}
								}
							}
						}
					}
					return nil, false
				}
				if args, found := findObject(data); found {
					return args, i
				}
				return data, i
			}
		}
	}
	return nil, 0
}
func (p *Parser) done() bool {
	var open, close rune
	switch p.tag {
	case "{":
		open, close = '{', '}'
	case "[":
		open, close = '[', ']'
	default:
		return false
	}
	var count int
	for _, c := range p.buffer {
		if c == byte(open) {
			count++
		} else if c == byte(close) {
			count--
			if count == 0 {
				return true
			}
		}
	}
	return false
}
func (p *Parser) Content() string {
	if p.n > 0 {
		return ""
	}
	if p.tag == "{" || p.tag == "[" {
		return string(p.buffer)
	}
	return ""
}