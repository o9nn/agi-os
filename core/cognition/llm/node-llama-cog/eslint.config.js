import path from "path";
import {fileURLToPath} from "url";
import importPlugin from "eslint-plugin-import";
import jsdoc from "eslint-plugin-jsdoc";
import n from "eslint-plugin-n";
import tseslint from "typescript-eslint";
import stylistic from "@stylistic/eslint-plugin";
import {includeIgnoreFile} from "@eslint/compat";
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const gitignorePath = path.join(__dirname, ".gitignore");
export default tseslint.config({
    ignores: [
        "dist/",
        "**/dist/",
        "**/dist-electron/",
        "llama/",
        "docs-site/",
        "templates/",
        ".vitepress/.cache/",
        "packages/create-node-llama-cpp/dist/",
        "packages/@node-llama-cpp**.{,c,m}{js,ts}"],
    extends: [
        stylistic.configs["recommended-flat"],
        jsdoc.configs["flat/recommended"],
        importPlugin.flatConfigs.recommended
    ],
    plugins: {
        n
    },
    languageOptions: {
        globals: {
            Atomics: "readonly",
            SharedArrayBuffer: "readonly"
        },
        ecmaVersion: 2023,
        sourceType: "module"
    },
    settings: {
        "import/resolver": {
            typescript: true,
            node: true
        },
        jsdoc: {
            exemptDestructuredRootsFromChecks: true,
            tagNamePreference: {
                hidden: "hidden",
                experimental: "experimental"
            }
        }
    },
    rules: {
        "@stylistic/indent": ["off"],
        "indent": ["warn", 4, {
            SwitchCase: 1,
            FunctionDeclaration: {
                parameters: "first"
            },
            ignoredNodes: [
                'FunctionExpression[params.length=0][returnType.type="TSTypeAnnotation"]'
            ]
        }],
        "@stylistic/indent-binary-ops": ["off"],
        "@stylistic/eqeqeq": ["off"],
        "@stylistic/no-undef": "off",
        "@stylistic/quotes": ["warn", "double", {avoidEscape: true}],
        "no-unused-vars": ["warn", {
            args: "none",
            ignoreRestSiblings: true,
            varsIgnorePattern: "^set",
            caughtErrors: "none"
        }],
        "@stylistic/no-prototype-builtins": ["off"],
        "@stylistic/object-curly-spacing": ["warn", "never"],
        "@stylistic/semi": ["warn", "always"],
        "@stylistic/no-undefined": ["off"],
        "@stylistic/array-bracket-newline": ["error", "consistent"],
        "@stylistic/brace-style": ["error", "1tbs", {
            allowSingleLine: false
        }],
        "@stylistic/comma-spacing": ["error", {
            before: false,
            after: true
        }],
        "@stylistic/comma-style": ["error", "last"],
        "@stylistic/comma-dangle": ["warn", "never"],
        "no-var": ["error"],
        "import/order": ["error", {
            groups: ["builtin", "external", "internal", "parent", "sibling", "index", "type", "object", "unknown"],
            warnOnUnassignedImports: true
        }],
        "n/file-extension-in-import": ["error", "always"],
        "newline-per-chained-call": ["error", {
            ignoreChainWithDepth: 2
        }],
        "no-confusing-arrow": ["error"],
        "no-const-assign": ["error"],
        "no-duplicate-imports": ["error", {
            includeExports: true
        }],
        camelcase: ["warn", {
            allow: ["\\d+_\\d+"]
        }],
        "@stylistic/jsx-quotes": ["warn"],
        yoda: ["error", "never", {
            exceptRange: true
        }],
        "no-eval": ["error"],
        "array-callback-return": ["error"],
        "no-empty": ["error", {
            allowEmptyCatch: true
        }],
        "@stylistic/keyword-spacing": ["warn"],
        "@stylistic/space-infix-ops": ["warn"],
        "@stylistic/spaced-comment": ["warn", "always", {
            markers: ["/"]
        }],
        "@stylistic/eol-last": ["warn", "always"],
        "@stylistic/max-len": ["warn", {
            code: 140,
            tabWidth: 4,
            ignoreStrings: true
        }],
        "@stylistic/quote-props": ["off"],
        "@stylistic/arrow-parens": ["warn", "always"],
        "@stylistic/no-multiple-empty-lines": ["off"],
        "@stylistic/operator-linebreak": ["off"],
        "@stylistic/block-spacing": ["warn", "never"],
        "@stylistic/no-extra-parens": ["off"],
        "@stylistic/padded-blocks": ["warn"],
        "@stylistic/multiline-ternary": ["off"],
        "@stylistic/lines-between-class-members": ["warn", {
            enforce: [
                {blankLine: "always", prev: "method", next: "*"},
                {blankLine: "always", prev: "*", next: "method"}
            ]
        }],
        "@stylistic/no-trailing-spaces": ["off"],
        "@stylistic/no-multi-spaces": ["warn"],
        "@stylistic/generator-star-spacing": ["off"]
    }
}, {
    files: ["****.ts"],
    rules: {
        "@stylistic/max-len": ["off"]
    }
});