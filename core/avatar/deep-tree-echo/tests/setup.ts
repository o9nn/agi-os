process.env.NODE_ENV = 'test';
process.env.PORT = '3001';
process.env.DEBUG = 'false';
jest.setTimeout(30000);
const originalConsole = { ...console };
beforeAll(() => {
  console.log = jest.fn();
  console.info = jest.fn();
  console.debug = jest.fn();
});
afterAll(() => {
  console.log = originalConsole.log;
  console.info = originalConsole.info;
  console.debug = originalConsole.debug;
});
declare global {
  namespace NodeJS {
    interface Global {
      wait: (ms: number) => Promise<void>;
    }
  }
}
(global as unknown as { wait: (ms: number) => Promise<void> }).wait = (ms: number) =>
  new Promise(resolve => setTimeout(resolve, ms));
afterEach(() => {
  jest.clearAllMocks();
});