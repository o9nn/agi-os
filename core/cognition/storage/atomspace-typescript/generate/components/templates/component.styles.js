module.exports = (componentName) => ({
content: `
import { createStyles } from '@mui/styles';
export const styles = ()=> createStyles({
root: {
},
content: {
height: '100%',
},
});
`,
extension: `styles.ts`
});