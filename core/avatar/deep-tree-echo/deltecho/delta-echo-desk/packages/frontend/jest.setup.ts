import '@testing-library/jest-dom'
declare global {
namespace jest {
interface Matchers<R> {
toBeInTheDocument(): R
toHaveTextContent(text: string): R
toHaveValue(value: string | number): R
toBeDisabled(): R
}
}
}
const localStorageMock = (() => {
let store: Record<string, string> = {}
return {
getItem: jest.fn((key: string) => store[key] || null),
setItem: jest.fn((key: string, value: string) => {
store[key] = value
}),
clear: jest.fn(() => {
store = {}
}),
removeItem: jest.fn((key: string) => {
delete store[key]
}),
}
})()
Object.defineProperty(window, 'localStorage', { value: localStorageMock })
window.confirm = jest.fn()
window.alert = jest.fn()
afterEach(() => {
jest.clearAllMocks()
})