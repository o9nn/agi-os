#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const ROADMAP_FILE = 'DEVO-GENESIS.md';
function validateRoadmap() {
console.log('🔍 Validating DEVO-GENESIS.md format for GitHub Actions workflow...\n');
if (!fs.existsSync(ROADMAP_FILE)) {
console.error(`❌ Error: ${ROADMAP_FILE} not found`);
process.exit(1);
}
const roadmapContent = fs.readFileSync(ROADMAP_FILE, 'utf8');
const nextStepsMatch = roadmapContent.match(/## Next Development Steps\n\n([\s\S]*?)(?=\n## |$)/);
if (!nextStepsMatch) {
console.error('❌ Error: No "Next Development Steps" section found');
console.log('📝 Expected format:');
console.log('## Next Development Steps\n\n1. **Timeline Name**:\n   - [ ] Task description');
process.exit(1);
}
console.log('✅ Found "Next Development Steps" section');
const nextStepsContent = nextStepsMatch[1];
const timelineRegex = /(\d+)\.\s\*\*([^*]+)\*\*:\s*\n((?:\s*-\s\[[^\]]*\][^\n]*\n?)*)/g;
const timelines = [];
let match;
while ((match = timelineRegex.exec(nextStepsContent)) !== null) {
const [, number, title, tasksText] = match;
const tasks = [];
const taskRegex = /\s*-\s\[([^\]]*)\]\s(.+)/g;
let taskMatch;
while ((taskMatch = taskRegex.exec(tasksText)) !== null) {
const [, status, description] = taskMatch;
tasks.push({
completed: status.trim() === 'x',
description: description.trim()
});
}
timelines.push({
number: parseInt(number),
title: title.trim(),
tasks: tasks
});
}
if (timelines.length === 0) {
console.error('❌ Error: No timeline sections found');
console.log('📝 Expected format:');
console.log('1. **Timeline Name**:\n   - [ ] Task description\n   - [x] Completed task');
process.exit(1);
}
console.log(`✅ Found ${timelines.length} timeline sections:`);
let totalTasks = 0;
let incompleteTasks = 0;
timelines.forEach(timeline => {
const incomplete = timeline.tasks.filter(task => !task.completed).length;
const complete = timeline.tasks.filter(task => task.completed).length;
console.log(`   ${timeline.number}. ${timeline.title}: ${timeline.tasks.length} tasks (${incomplete} pending, ${complete} complete)`);
totalTasks += timeline.tasks.length;
incompleteTasks += incomplete;
timeline.tasks.forEach(task => {
if (task.description.length < 5) {
console.warn(`⚠️  Warning: Short task description in "${timeline.title}": "${task.description}"`);
}
});
});
console.log(`\n📊 Summary:`);
console.log(`   • Total tasks: ${totalTasks}`);
console.log(`   • Pending tasks: ${incompleteTasks}`);
console.log(`   • Completed tasks: ${totalTasks - incompleteTasks}`);
if (incompleteTasks === 0) {
console.log('\n🎉 All tasks are marked as complete! No issues will be created.');
} else {
console.log(`\n🚀 GitHub Actions will create ${incompleteTasks} issues for pending tasks`);
}
console.log(`\n🏷️  Timeline labels that will be created:`);
timelines.forEach(timeline => {
const label = timeline.title.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
console.log(`   • "${timeline.title}" → "${label}"`);
});
console.log('\n✅ DEVO-GENESIS.md format validation passed!');
console.log('🔄 The generate-next-steps.yml workflow should work correctly with this format.');
}
if (require.main === module) {
validateRoadmap();
}
module.exports = { validateRoadmap };