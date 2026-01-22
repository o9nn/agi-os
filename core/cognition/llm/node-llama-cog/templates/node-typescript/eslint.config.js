import importPlugin from "eslint-plugin-import";
import jsdoc from "eslint-plugin-jsdoc";
import n from "eslint-plugin-n";
import tseslint from "typescript-eslint";
import stylistic from "@stylistic/eslint-plugin";
export default tseslint.config({
    ignores: ["dist/", "models/"]
}, {
    files: ["****.{,c,m}ts"],
    extends: [
        jsdoc.configs["flat/recommended-typescript"],
        ...tseslint.configs.recommended
    ],
    settings: {
        "import/resolver": {
            typescript: true,
            node: true
        }
    },
    rules: {
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
        "jsdoc/require-param": ["off"],
        "jsdoc/check-param-names": ["warn", {
            checkDestructured: false
        }],
        "jsdoc/require-returns": ["off"],
        "jsdoc/require-jsdoc": ["off"],
        "jsdoc/require-yields": ["off"],
        "jsdoc/require-param-description": ["off"]
    }
});