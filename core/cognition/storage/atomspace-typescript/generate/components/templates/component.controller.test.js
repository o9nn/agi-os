module.exports = (componentName) => ({
content: `
import ${componentName}Controller from "../${componentName}.controller";
describe("${componentName} Function Unit Test", () => {
beforeEach(() => {
});
it("Brand new test for function should fail.", () => {
let input = "arbitrary";
let result = ${componentName}Controller.init();
expect(result).toEqual("updated");
});
});
`,
extension: `.test.tsx`,
functionsTest: true
});