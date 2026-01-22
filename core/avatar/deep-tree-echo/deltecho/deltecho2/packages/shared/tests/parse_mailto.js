import { describe } from 'mocha'
import { expect } from 'chai'
import { parseMailto } from '../ts-compiled-for-tests/parse_mailto.js'
describe('/shared/parse_mailto', () => {
it('mailto-parsing', () => {
expect(parseMailto('mailto:ex@example.com?body=hello')).to.deep.eq({
to: 'ex@example.com',
subject: undefined,
body: 'hello',
})
expect(
parseMailto('mailto:ex@example.com?body=hello&subject=hi')
).to.deep.eq({
to: 'ex@example.com',
subject: 'hi',
body: 'hello',
})
expect(parseMailto('mailto:?body=hello&subject=hi')).to.deep.eq({
to: null,
subject: 'hi',
body: 'hello',
})
expect(parseMailto('mailto:?body=hello')).to.deep.eq({
to: null,
subject: undefined,
body: 'hello',
})
expect(parseMailto('mailto:?body=hello%20world')).to.deep.eq({
to: null,
subject: undefined,
body: 'hello world',
})
expect(
parseMailto('mailto:deltabot@example.com?body=/web+https://github.com/')
).to.deep.eq({
to: 'deltabot@example.com',
subject: undefined,
body: '/web https://github.com/',
})
})
})