#!/usr/bin/env node
const fs = require('fs');
const ROADMAP_FILE = 'DEVO-GENESIS.md';
console.log('🔍 Validating DEVO-GENESIS.md format for automated issue generation...\n');
try {
    const roadmapContent = fs.readFileSync(ROADMAP_FILE, 'utf8');
    const nextStepsMatch = roadmapContent.match(/## Next Development Steps\n\n([\s\S]*?)(?=\n## |$)/);
    if (!nextStepsMatch) {
        console.error('❌ No "Next Development Steps" section found');
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
    console.log(`✅ Found ${timelines.length} timeline sections`);
    let totalTasks = 0;
    let incompleteTasks = 0;
    for (const timeline of timelines) {
        const incomplete = timeline.tasks.filter(task => !task.completed).length;
        console.log(`  📋 ${timeline.title}: ${timeline.tasks.length} tasks (${incomplete} incomplete)`);
        totalTasks += timeline.tasks.length;
        incompleteTasks += incomplete;
    }
    console.log(`\n📊 Summary:`);
    console.log(`  • Total tasks: ${totalTasks}`);
    console.log(`  • Incomplete tasks: ${incompleteTasks}`);
    console.log(`  • Tasks that would generate issues: ${incompleteTasks}`);
    console.log(`\n🎫 Sample issue titles that would be generated:`);
    for (const timeline of timelines.slice(0, 2)) { 
        for (const task of timeline.tasks.slice(0, 2)) { 
            if (!task.completed) {
                const issueTitle = `[${timeline.title}] ${task.description}`;
                console.log(`  • "${issueTitle}"`);
            }
        }
    }
    const formatIssues = [];
    for (const timeline of timelines) {
        if (!timeline.title || timeline.title.length === 0) {
            formatIssues.push(`Timeline ${timeline.number} has empty title`);
        }
        for (const task of timeline.tasks) {
            if (!task.description || task.description.length === 0) {
                formatIssues.push(`Empty task description in ${timeline.title}`);
            }
            if (task.description.length > 100) {
                formatIssues.push(`Task description too long in ${timeline.title}: "${task.description.substring(0, 50)}..."`);
            }
        }
    }
    if (formatIssues.length > 0) {
        console.log(`\n⚠️  Format issues found:`);
        formatIssues.forEach(issue => console.log(`  • ${issue}`));
    } else {
        console.log(`\n✅ Format validation passed - roadmap is compatible with automated issue generation`);
    }
    console.log(`\n🔄 To test the workflow:`);
    console.log(`  1. Navigate to GitHub Actions in the repository`);
    console.log(`  2. Find "Generate Next Steps Issues" workflow`);
    console.log(`  3. Click "Run workflow" to test issue generation`);
    console.log(`  4. Check generated issues in the Issues tab`);
} catch (error) {
    console.error(`❌ Error validating roadmap: ${error.message}`);
    process.exit(1);
}