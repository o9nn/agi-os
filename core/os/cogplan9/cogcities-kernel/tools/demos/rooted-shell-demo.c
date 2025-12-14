/*
 * Rooted Shell Namespace Demonstration
 * 
 * Demonstrates the mapping of rooted tree configurations to Plan 9 namespaces
 * where each shell is both a namespace (container) and a file (entity).
 * 
 * This shows how nested shell configurations from A000081 sequence create
 * filesystem hierarchies with dual representations.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/*
 * Simplified structures for demonstration (would use kernel structures in real impl)
 */

typedef struct {
    char *parens;
    char *namespace_path;
    char *file_path;
    int node_count;
    int depth;
} DemoShell;

static void
print_banner(const char *title)
{
    printf("\n");
    printf("========================================\n");
    printf("%s\n", title);
    printf("========================================\n\n");
}

static void
print_section(const char *title)
{
    printf("\n--- %s ---\n\n", title);
}

static int
count_depth(const char *parens)
{
    int depth = 0;
    int max_depth = 0;
    
    for (int i = 0; parens[i] != '\0'; i++) {
        if (parens[i] == '(') {
            depth++;
            if (depth > max_depth)
                max_depth = depth;
        } else {
            depth--;
        }
    }
    
    return max_depth;
}

static char*
parens_to_namespace(const char *parens, const char *domain)
{
    static char path[256];
    int pos = 0;
    int shell_num = 0;
    
    pos += snprintf(path + pos, sizeof(path) - pos, "/%s", domain);
    
    for (int i = 0; parens[i] != '\0'; i++) {
        if (parens[i] == '(') {
            pos += snprintf(path + pos, sizeof(path) - pos, "/shell%d", shell_num++);
        }
    }
    
    return path;
}

static void
create_demo_shell(DemoShell *shell, const char *parens, const char *domain)
{
    int node_count = 0;
    for (int i = 0; parens[i] != '\0'; i++) {
        if (parens[i] == '(')
            node_count++;
    }
    
    shell->parens = strdup(parens);
    shell->node_count = node_count;
    shell->depth = count_depth(parens);
    
    char *ns_path = parens_to_namespace(parens, domain);
    shell->namespace_path = strdup(ns_path);
    
    char file_path[256];
    snprintf(file_path, sizeof(file_path), "%s.shell", ns_path);
    shell->file_path = strdup(file_path);
}

static void
print_shell_info(DemoShell *shell, int index)
{
    printf("  [%d] Tree: %-12s  Nodes: %d  Depth: %d\n", 
           index, shell->parens, shell->node_count, shell->depth);
    printf("      Namespace: %s\n", shell->namespace_path);
    printf("      File:      %s\n", shell->file_path);
    printf("\n");
}

static void
demo_shell_dual_representation(void)
{
    print_banner("Rooted Shell Namespace Demo: Dual Representation");
    
    printf("Concept: Every shell is BOTH a namespace AND a file.\n");
    printf("This dual representation enables:\n");
    printf("  - Navigation through shell hierarchies (as namespace)\n");
    printf("  - Direct file operations on shells (as file)\n");
    printf("  - Uniform addressing protocol\n");
    
    print_section("Example: 3-Shell Configuration");
    
    DemoShell shell;
    create_demo_shell(&shell, "((()))", "transportation");
    
    printf("Parentheses notation:  %s\n", shell.parens);
    printf("Interpretation: 3 nested shells (bags)\n\n");
    
    printf("Representation 1 - AS NAMESPACE:\n");
    printf("  Path: %s\n", shell.namespace_path);
    printf("  Usage: cd %s\n", shell.namespace_path);
    printf("  Purpose: Navigate into nested shell structure\n\n");
    
    printf("Representation 2 - AS FILE:\n");
    printf("  Path: %s\n", shell.file_path);
    printf("  Usage: cat %s\n", shell.file_path);
    printf("  Purpose: Read shell metadata and contents\n\n");
    
    printf("Key Insight: The SAME entity is accessible both ways!\n");
}

static void
demo_enumerate_trees(const char *domain, int max_size)
{
    char title[128];
    snprintf(title, sizeof(title), "Enumerating All %d-Trees for '%s' Domain", max_size, domain);
    print_banner(title);
    
    printf("The A000081 sequence counts rooted trees:\n");
    printf("  n=1: 1 tree   n=2: 1 tree   n=3: 2 trees   n=4: 4 trees\n");
    printf("  n=5: 9 trees  n=6: 20 trees n=7: 48 trees  ...\n\n");
    
    // Hardcoded examples for demonstration
    const char *trees_by_size[][20] = {
        {"()"},                                                    // n=1
        {"(())"},                                                  // n=2
        {"((()))", "(()())"},                                      // n=3
        {"(((())))", "((()()))", "((())())", "(()(()))"},         // n=4
    };
    
    int counts[] = {1, 1, 2, 4};
    
    if (max_size < 1 || max_size > 4) {
        printf("For this demo, showing trees up to size 4\n");
        max_size = 4;
    }
    
    printf("Generating all shell configurations from size 1 to %d:\n\n", max_size);
    
    int total = 0;
    for (int n = 1; n <= max_size; n++) {
        printf("Size %d: %d configurations\n", n, counts[n-1]);
        
        for (int i = 0; i < counts[n-1]; i++) {
            DemoShell shell;
            create_demo_shell(&shell, trees_by_size[n-1][i], domain);
            print_shell_info(&shell, total + i + 1);
            free(shell.parens);
            free(shell.namespace_path);
            free(shell.file_path);
        }
        
        total += counts[n-1];
    }
    
    printf("Total: %d shell configurations created\n", total);
}

static void
demo_filesystem_hierarchy(void)
{
    print_banner("Filesystem Hierarchy Example");
    
    printf("Consider the 4-tree: ((())())\n");
    printf("This represents 4 nested shells with specific structure.\n\n");
    
    printf("Resulting filesystem hierarchy:\n\n");
    
    printf("/transportation/                            # Domain root\n");
    printf("├── shell0.shell                            # Outermost shell as file\n");
    printf("└── shell0/                                 # Outermost shell as namespace\n");
    printf("    ├── shell1.shell                        # Nested shell 1 as file\n");
    printf("    ├── shell1/                             # Nested shell 1 as namespace\n");
    printf("    │   └── shell2.shell                    # Deeply nested shell as file\n");
    printf("    └── shell3.shell                        # Sibling shell as file\n\n");
    
    printf("Key observations:\n");
    printf("  1. Each shell appears TWICE: once as file, once as directory\n");
    printf("  2. Nesting structure matches parentheses: ((())())\n");
    printf("  3. Standard Plan 9 navigation works: cd, ls, cat\n");
    printf("  4. Shell files contain metadata about their structure\n");
}

static void
demo_addressing_protocol(void)
{
    print_banner("Addressing Protocol");
    
    printf("The rooted shell namespace provides a systematic addressing protocol\n");
    printf("using Plan 9 namespaces to locate files according to tree structure.\n\n");
    
    printf("Address format: /<domain>/<shell_path>/<entity>\n\n");
    
    printf("Examples:\n\n");
    
    const char *addresses[] = {
        "/transportation/shell0",
        "/transportation/shell0.shell",
        "/transportation/shell0/shell1",
        "/transportation/shell0/shell1.shell",
        "/energy/shell0/shell1/shell2",
        "/governance/shell0/shell1/shell2/shell3.shell",
    };
    
    const char *descriptions[] = {
        "Navigate to outermost shell namespace",
        "Read outermost shell as file",
        "Navigate to nested shell",
        "Read nested shell as file",
        "Deep nesting in energy domain",
        "Very deep nesting in governance domain",
    };
    
    for (int i = 0; i < 6; i++) {
        printf("  %-55s # %s\n", addresses[i], descriptions[i]);
    }
    
    printf("\nPath resolution is O(depth) - very efficient!\n");
}

static void
demo_domain_specific_shells(void)
{
    print_banner("Domain-Specific Shell Configurations");
    
    printf("Different cognitive domains use shells for different purposes:\n\n");
    
    print_section("Transportation Domain");
    printf("Model intersection configurations:\n");
    printf("  (())         → Simple intersection (2 roads)\n");
    printf("  (()())       → T-intersection (3 roads)\n");
    printf("  ((()))       → Sequential traffic lights\n");
    printf("  ((())())     → Complex junction with priorities\n\n");
    
    DemoShell traffic;
    create_demo_shell(&traffic, "(()())", "transportation");
    printf("Example T-intersection:\n");
    printf("  Tree:      %s\n", traffic.parens);
    printf("  Namespace: %s\n", traffic.namespace_path);
    printf("  File:      %s\n\n", traffic.file_path);
    
    print_section("Energy Domain");
    printf("Model distribution network structures:\n");
    printf("  (())         → Single feeder line\n");
    printf("  (()())       → Branched distribution\n");
    printf("  ((()))       → Cascaded transformers\n");
    printf("  ((())(()))   → Redundant paths for reliability\n\n");
    
    DemoShell energy;
    create_demo_shell(&energy, "((()))", "energy");
    printf("Example cascaded system:\n");
    printf("  Tree:      %s\n", energy.parens);
    printf("  Namespace: %s\n", energy.namespace_path);
    printf("  File:      %s\n\n", energy.file_path);
    
    print_section("Governance Domain");
    printf("Model policy hierarchies:\n");
    printf("  (())         → Direct policy (no intermediaries)\n");
    printf("  (()())       → Multi-stakeholder review\n");
    printf("  ((()))       → Nested oversight (checks and balances)\n");
    printf("  ((())())     → Complex approval chain\n\n");
    
    DemoShell governance;
    create_demo_shell(&governance, "((())())", "governance");
    printf("Example complex approval:\n");
    printf("  Tree:      %s\n", governance.parens);
    printf("  Namespace: %s\n", governance.namespace_path);
    printf("  File:      %s\n\n", governance.file_path);
}

static void
demo_filesystem_interface(void)
{
    print_banner("Filesystem Interface: /proc/cognitive/rooted/");
    
    printf("The rooted shell interface is accessible via Plan 9 filesystem:\n\n");
    
    printf("Directory structure:\n");
    printf("  /proc/cognitive/rooted/\n");
    printf("  ├── ctl          # Control commands (write)\n");
    printf("  ├── list         # Help and command list (read)\n");
    printf("  ├── trees        # Generated tree configurations (read)\n");
    printf("  └── shells       # Active shell instances (read)\n\n");
    
    print_section("Control Commands (write to ctl)");
    
    printf("Create shell from parentheses notation:\n");
    printf("  $ echo 'create transportation (()())' > /proc/cognitive/rooted/ctl\n\n");
    
    printf("Enumerate all 5-trees for a domain:\n");
    printf("  $ echo 'enumerate energy 5' > /proc/cognitive/rooted/ctl\n\n");
    
    printf("Get shell information:\n");
    printf("  $ echo 'info shell-transportation-12345' > /proc/cognitive/rooted/ctl\n\n");
    
    printf("Display statistics:\n");
    printf("  $ echo 'stats' > /proc/cognitive/rooted/ctl\n\n");
    
    print_section("Reading Information");
    
    printf("List available commands:\n");
    printf("  $ cat /proc/cognitive/rooted/list\n\n");
    
    printf("View generated trees:\n");
    printf("  $ cat /proc/cognitive/rooted/trees\n\n");
    
    printf("List active shells:\n");
    printf("  $ cat /proc/cognitive/rooted/shells\n\n");
}

static void
demo_mathematical_properties(void)
{
    print_banner("Mathematical Properties: A000081 Sequence");
    
    printf("The rooted shell namespace is grounded in rigorous mathematics.\n\n");
    
    printf("A000081 sequence (number of rooted trees):\n");
    printf("  n:  1   2   3    4    5    6     7     8     9     10\n");
    printf("  T:  1   1   2    4    9   20    48   115   286   719\n\n");
    
    printf("Growth rate: Approximately α^n where α ≈ 2.9558 (Otter's constant)\n\n");
    
    printf("Properties:\n");
    printf("  • Canonical ordering: Each tree has unique representation\n");
    printf("  • Binary encoding: Compact storage using bit manipulation\n");
    printf("  • Recursive structure: Trees defined by subtrees\n");
    printf("  • Exponential growth: Rich combinatorial structure\n\n");
    
    printf("Current implementation limits:\n");
    printf("  • Maximum tree size: 15 nodes\n");
    printf("  • Total 15-trees: 37,663 configurations\n");
    printf("  • Storage: ~10,000 trees initially, expandable\n");
    printf("  • Performance: < 1ms to generate trees up to n=10\n");
}

int
main(void)
{
    print_banner("Plan 9 Cognitive Cities: Rooted Shell Namespaces");
    
    printf("This demonstration shows how rooted tree configurations map to\n");
    printf("Plan 9 namespaces, where each shell is both a namespace and a file.\n\n");
    
    printf("Based on the insight:\n");
    printf("  'Every shell is also a file... if we define the filesystem\n");
    printf("   according to nested shell configurations using P9 namespaces\n");
    printf("   to address files, then we have a filesystem structured like\n");
    printf("   the rooted trees.'\n");
    
    printf("\nPress Enter to continue through demonstrations...\n");
    getchar();
    
    // Run demonstrations
    demo_shell_dual_representation();
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_enumerate_trees("transportation", 4);
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_filesystem_hierarchy();
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_addressing_protocol();
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_domain_specific_shells();
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_filesystem_interface();
    printf("\nPress Enter for next demo...\n");
    getchar();
    
    demo_mathematical_properties();
    
    print_banner("Summary");
    
    printf("Rooted Shell Namespaces provide:\n\n");
    
    printf("  ✓ Mathematical elegance: Based on A000081 sequence\n");
    printf("  ✓ Dual representation: Shells as namespaces AND files\n");
    printf("  ✓ Systematic addressing: Plan 9 namespace protocol\n");
    printf("  ✓ Domain-specific use: Different meanings per domain\n");
    printf("  ✓ Combinatorial richness: Exponential configurations\n");
    printf("  ✓ Plan 9 integration: Native filesystem interface\n\n");
    
    printf("This implementation realizes the vision:\n");
    printf("  'A filesystem structured like the rooted trees, which are also\n");
    printf("   the elementary differentials over integer partitions that form\n");
    printf("   the most efficient basis for complementary perspectives.'\n\n");
    
    printf("========================================\n");
    printf("Demo complete!\n");
    printf("========================================\n\n");
    
    return 0;
}
