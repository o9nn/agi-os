#!/usr/bin/env node
/**
 * Functionality Verification Suite for smallest-agent.js
 * 
 * This test suite verifies that the minified agent preserves all functionality:
 * 1. Module imports are valid (Anthropic SDK, child_process, readline)
 * 2. Syntax is valid JavaScript ES modules
 * 3. Code structure matches expected patterns
 * 4. All required components are present
 */

import { readFileSync } from 'fs';
import { execSync } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Test configuration
const REQUIRED_IMPORTS = [
  '@anthropic-ai/sdk',
  'child_process',
  'readline'
];

const REQUIRED_PATTERNS = [
  /process\.env\.API_KEY/,           // API key from environment
  /messages\.create/,                 // Anthropic API call
  /claude-sonnet-4-20250514/,        // Model name
  /max_tokens/,                       // Token limit
  /tools:\[/,                         // Tool definition
  /name:"bash"/,                      // Bash tool name
  /execSync/,                         // Command execution
  /tool_result/,                      // Tool result handling
  /createInterface/,                  // Readline interface
  /question\(/,                       // User input prompt
];

const REQUIRED_FUNCTIONALITY = [
  { name: 'User input loop', pattern: /for\s*\(/ },
  { name: 'Message history', pattern: /push\s*\(\s*\{role:/ },
  { name: 'Tool detection', pattern: /tool_use/ },
  { name: 'Error handling', pattern: /try\s*\{.*catch/ },
  { name: 'Console output', pattern: /console\.log/ },
];

function runTests(filePath) {
  console.log(`\n🧪 Testing: ${filePath}\n`);
  
  let code;
  try {
    code = readFileSync(filePath, 'utf-8');
  } catch (e) {
    console.error(`❌ Failed to read file: ${e.message}`);
    return false;
  }
  
  const results = {
    passed: 0,
    failed: 0,
    tests: []
  };
  
  // Test 1: Syntax validation
  console.log('📋 Test 1: Syntax Validation');
  try {
    execSync(`node -c "${filePath}"`, { encoding: 'utf-8' });
    console.log('  ✅ Valid JavaScript syntax');
    results.passed++;
    results.tests.push({ name: 'Syntax', passed: true });
  } catch (e) {
    console.log(`  ❌ Invalid syntax: ${e.message}`);
    results.failed++;
    results.tests.push({ name: 'Syntax', passed: false, error: e.message });
  }
  
  // Test 2: Required imports
  console.log('\n📋 Test 2: Required Imports');
  for (const imp of REQUIRED_IMPORTS) {
    if (code.includes(imp)) {
      console.log(`  ✅ Found import: ${imp}`);
      results.passed++;
      results.tests.push({ name: `Import: ${imp}`, passed: true });
    } else {
      console.log(`  ❌ Missing import: ${imp}`);
      results.failed++;
      results.tests.push({ name: `Import: ${imp}`, passed: false });
    }
  }
  
  // Test 3: Required patterns
  console.log('\n📋 Test 3: Required Patterns');
  for (const pattern of REQUIRED_PATTERNS) {
    if (pattern.test(code)) {
      console.log(`  ✅ Found pattern: ${pattern.source.substring(0, 30)}...`);
      results.passed++;
      results.tests.push({ name: `Pattern: ${pattern.source.substring(0, 20)}`, passed: true });
    } else {
      console.log(`  ❌ Missing pattern: ${pattern.source}`);
      results.failed++;
      results.tests.push({ name: `Pattern: ${pattern.source.substring(0, 20)}`, passed: false });
    }
  }
  
  // Test 4: Required functionality
  console.log('\n📋 Test 4: Required Functionality');
  for (const func of REQUIRED_FUNCTIONALITY) {
    if (func.pattern.test(code)) {
      console.log(`  ✅ Found: ${func.name}`);
      results.passed++;
      results.tests.push({ name: func.name, passed: true });
    } else {
      console.log(`  ❌ Missing: ${func.name}`);
      results.failed++;
      results.tests.push({ name: func.name, passed: false });
    }
  }
  
  // Test 5: Size measurement
  console.log('\n📋 Test 5: Size Measurement');
  const size = Buffer.byteLength(code, 'utf-8');
  console.log(`  📏 File size: ${size} bytes`);
  results.tests.push({ name: 'Size', value: size });
  
  // Summary
  console.log('\n' + '='.repeat(50));
  console.log(`📊 Results: ${results.passed} passed, ${results.failed} failed`);
  console.log('='.repeat(50));
  
  return results.failed === 0;
}

// Main execution
const args = process.argv.slice(2);
if (args.length === 0) {
  console.log('Usage: node test-functionality.js <file.js>');
  console.log('Example: node test-functionality.js smallest-agent-v1.js');
  process.exit(1);
}

const success = runTests(args[0]);
process.exit(success ? 0 : 1);
