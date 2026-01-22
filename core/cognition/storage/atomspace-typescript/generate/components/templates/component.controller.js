module.exports = (componentName) => ({
content: `
const ${componentName}Controller = {
init: () => {
}
}
export default ${componentName}Controller;
`,
extension: `.ts`,
functions: true
});