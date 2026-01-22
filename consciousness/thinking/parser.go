package thinking
import (
"strings"
"unicode"
)
type thinkingState int
const (
thinkingState_LookingForOpening thinkingState = iota
thinkingState_ThinkingStartedEatingWhitespace
thinkingState_Thinking
thinkingState_ThinkingDoneEatingWhitespace
thinkingState_ThinkingDone
)
func (s thinkingState) String() string {
switch s {
case thinkingState_LookingForOpening:
return "LookingForOpening"
case thinkingState_ThinkingStartedEatingWhitespace:
return "ThinkingStartedEatingWhitespace"
case thinkingState_Thinking:
return "Thinking"
case thinkingState_ThinkingDoneEatingWhitespace:
return "ThinkingDoneEatingWhitespace"
case thinkingState_ThinkingDone:
return "ThinkingDone"
default:
return "Unknown"
}
}
type Parser struct {
state      thinkingState
OpeningTag string
ClosingTag string
acc        strings.Builder
}
func (s *Parser) AddContent(content string) (string, string) {
s.acc.WriteString(content)
var thinkingSb, remainingSb strings.Builder
var thinking, remaining string
keepLooping := true
for keepLooping {
thinking, remaining, keepLooping = eat(s)
thinkingSb.WriteString(thinking)
remainingSb.WriteString(remaining)
}
return thinkingSb.String(), remainingSb.String()
}
func eat(s *Parser) (string, string, bool) {
switch s.state {
case thinkingState_LookingForOpening:
trimmed := strings.TrimLeftFunc(s.acc.String(), unicode.IsSpace)
if strings.HasPrefix(trimmed, s.OpeningTag) {
after := strings.Join(strings.Split(trimmed, s.OpeningTag)[1:], s.OpeningTag)
after = strings.TrimLeftFunc(after, unicode.IsSpace)
s.acc.Reset()
s.acc.WriteString(after)
if after == "" {
s.state = thinkingState_ThinkingStartedEatingWhitespace
} else {
s.state = thinkingState_Thinking
}
return "", "", true
} else if strings.HasPrefix(s.OpeningTag, trimmed) {
return "", "", false
} else if trimmed == "" {
return "", "", false
} else {
s.state = thinkingState_ThinkingDone
return "", s.acc.String(), false
}
case thinkingState_ThinkingStartedEatingWhitespace:
trimmed := strings.TrimLeftFunc(s.acc.String(), unicode.IsSpace)
s.acc.Reset()
if trimmed == "" {
return "", "", false
} else {
s.state = thinkingState_Thinking
s.acc.WriteString(trimmed)
return "", "", true
}
case thinkingState_Thinking:
acc := s.acc.String()
if strings.Contains(acc, s.ClosingTag) {
split := strings.Split(acc, s.ClosingTag)
thinking := split[0]
remaining := strings.Join(split[1:], s.ClosingTag)
remaining = strings.TrimLeftFunc(remaining, unicode.IsSpace)
s.acc.Reset()
if remaining == "" {
s.state = thinkingState_ThinkingDoneEatingWhitespace
} else {
s.state = thinkingState_ThinkingDone
}
return thinking, remaining, false
} else if overlapLen := overlap(acc, s.ClosingTag); overlapLen > 0 {
thinking := acc[:len(acc)-overlapLen]
remaining := acc[len(acc)-overlapLen:]
s.acc.Reset()
s.acc.WriteString(remaining)
return thinking, "", false
} else {
s.acc.Reset()
return acc, "", false
}
case thinkingState_ThinkingDoneEatingWhitespace:
trimmed := strings.TrimLeftFunc(s.acc.String(), unicode.IsSpace)
s.acc.Reset()
if trimmed != "" {
s.state = thinkingState_ThinkingDone
}
return "", trimmed, false
case thinkingState_ThinkingDone:
acc := s.acc.String()
s.acc.Reset()
return "", acc, false
default:
panic("unknown state")
}
}
func overlap(s, delim string) int {
max := min(len(delim), len(s))
for i := max; i > 0; i-- {
if strings.HasSuffix(s, delim[:i]) {
return i
}
}
return 0
}