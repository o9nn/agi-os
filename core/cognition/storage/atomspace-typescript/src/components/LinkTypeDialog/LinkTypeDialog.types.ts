import {BaseProps} from "../../util/BaseProps";
import {StyledComponentProps} from "@mui/styles";
import React from "react";
interface LinkTypeDialogProps extends BaseProps, StyledComponentProps {
  testId?: string;
  children?: React.ReactNode;
  className?: string;
  options: string[];
  initialValue?: string;
  open: boolean;
  onCancel: () => void;
  onSubmit: (value: string) => void;
}
export default LinkTypeDialogProps;