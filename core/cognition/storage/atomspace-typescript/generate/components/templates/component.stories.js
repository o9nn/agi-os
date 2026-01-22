module.exports = (componentName, componentType) => ({
  content: `
import React from "react";
import ${componentName} from "../${componentName}";
import { ThemeProvider } from "@mui/styles";
import theme from "../../../util/theme";
import StorybookWrapper from "../../../util/StorybookWrapper";
export default {
    title: "${componentType}/${componentName}"
};
export const Default = () => 
<ThemeProvider theme={theme}>
  <StorybookWrapper fill>
     <${componentName} />
  </StorybookWrapper>;
</ThemeProvider>;
`,
  extension: `.stories.tsx`
});