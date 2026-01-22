import importPlugin from "eslint-plugin-import";
import jsdoc from "eslint-plugin-jsdoc";
import reactRefresh from "eslint-plugin-react-refresh";
import tseslint from "typescript-eslint";
import stylistic from "@stylistic/eslint-plugin";
import pluginReactHooks from "eslint-plugin-react-hooks";
export default tseslint.config({
    ignores: ["dist/", "dist-electron/", "release/", "models/"]
}, {
    files: ["****.{ts,tsx}"],
    extends: [
        jsdoc.configs["flat/recommended-typescript"],
        ...tseslint.configs.recommended
    ],
    plugins: {
        "react-hooks": pluginReactHooks,
        "react-refresh": reactRefresh
    },
    settings: {
        "import/resolver": {
            typescript: true,
            node: true
        }
    },
    rules: {
        ...pluginReactHooks.configs.recommended.rules,
        "no-constant-condition": ["warn"],
        "import/named": ["off"],
        "@typescript-eslint/explicit-module-boundary-types": ["off"],
        "@typescript-eslint/ban-ts-comment": ["off"],
        "@typescript-eslint/no-explicit-any": ["off"],
        "@typescript-eslint/no-inferrable-types": ["off"],
        "@typescript-eslint/no-unused-vars": ["warn", {
            args: "none",
            ignoreRestSiblings: true,
            varsIgnorePattern: "^set",
            caughtErrors: "none"
        }],
        "@typescript-eslint/no-empty-object-type": ["off"],
        "@typescript-eslint/member-ordering": ["warn", {
            default: ["field", "constructor", "method", "signature"],
            typeLiterals: []
        }],
        "@typescript-eslint/parameter-properties": ["warn", {
            allow: []
        }],
        "@typescript-eslint/explicit-member-accessibility": ["warn"],
        "@stylistic/member-delimiter-style": ["warn", {
            multiline: {
                delimiter: "comma",
                requireLast: false
            },
            singleline: {
                delimiter: "comma",
                requireLast: false
            },
            multilineDetection: "brackets"
        }],
        "@stylistic/jsx-wrap-multilines": ["off"],
        "@stylistic/jsx-indent-props": ["warn", 4],
        "@stylistic/jsx-one-expression-per-line": ["off"],
        "@stylistic/jsx-closing-tag-location": ["warn", "line-aligned"],
        "@stylistic/jsx-closing-bracket-location": ["warn", "line-aligned"],
        "@stylistic/jsx-tag-spacing": ["warn"],
        "jsdoc/require-param": ["off"],
        "jsdoc/check-param-names": ["warn", {
            checkDestructured: false
        }],
        "jsdoc/require-returns": ["off"],
        "jsdoc/require-jsdoc": ["off"],
        "jsdoc/require-yields": ["off"],
        "jsdoc/require-param-description": ["off"],
        "react-refresh/only-export-components": ["warn", {
            "allowConstantExport": true
        }],
        "react-hooks/exhaustive-deps": ["off"]
    }
});