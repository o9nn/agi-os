/**
 * Jest Test Setup
 *
 * Global configuration and mocks for test environment.
 */

// Set test environment variables
process.env.NODE_ENV = 'test';
process.env.PORT = '3001';
process.env.DEBUG = 'false';

// Increase timeout for async tests
jest.setTimeout(30000);

// Mock console to reduce noise in tests
const originalConsole = { ...console };

beforeAll(() => {
  console.log = jest.fn();
  console.info = jest.fn();
  console.debug = jest.fn();
  // Keep error and warn for debugging
  // console.error = jest.fn();
  // console.warn = jest.fn();
});

afterAll(() => {
  console.log = originalConsole.log;
  console.info = originalConsole.info;
  console.debug = originalConsole.debug;
});

// Global test utilities
declare global {
  namespace NodeJS {
    interface Global {
      wait: (ms: number) => Promise<void>;
    }
  }
}

(global as unknown as { wait: (ms: number) => Promise<void> }).wait = (ms: number) =>
  new Promise(resolve => setTimeout(resolve, ms));

// Clean up after each test
afterEach(() => {
  jest.clearAllMocks();
});
