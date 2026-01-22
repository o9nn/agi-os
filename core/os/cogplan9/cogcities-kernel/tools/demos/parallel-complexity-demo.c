#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#define MAX_VARS 20
#define MAX_CLAUSES 100
typedef struct {
int num_vars;
int num_clauses;
int clauses[MAX_CLAUSES][3];
} SATProblem;
typedef struct {
long long sequential_steps;
long long parallel_steps;
long long membranes_created;
double sequential_time_sec;
double parallel_time_sec;
} ExecutionStats;
SATProblem generate_sat_problem(int num_vars, int num_clauses) {
SATProblem prob;
prob.num_vars = num_vars;
prob.num_clauses = num_clauses;
for (int i = 0; i < num_clauses; i++) {
for (int j = 0; j < 3; j++) {
int var = (rand() % num_vars) + 1;
int polarity = (rand() % 2) ? 1 : -1;
prob.clauses[i][j] = var * polarity;
}
}
return prob;
}
int satisfies_clause(int *assignment, int *clause, int num_vars) {
for (int i = 0; i < 3; i++) {
int lit = clause[i];
int var = abs(lit) - 1;
int polarity = (lit > 0) ? 1 : 0;
if (assignment[var] == polarity) {
return 1;
}
}
return 0;
}
int satisfies_formula(int *assignment, SATProblem *prob) {
for (int i = 0; i < prob->num_clauses; i++) {
if (!satisfies_clause(assignment, prob->clauses[i], prob->num_vars)) {
return 0;
}
}
return 1;
}
int solve_sat_sequential(SATProblem *prob, ExecutionStats *stats) {
int num_vars = prob->num_vars;
long long total_assignments = 1LL << num_vars;
int assignment[MAX_VARS];
clock_t start = clock();
stats->sequential_steps = 0;
for (long long i = 0; i < total_assignments; i++) {
for (int j = 0; j < num_vars; j++) {
assignment[j] = (i >> j) & 1;
}
stats->sequential_steps++;
if (satisfies_formula(assignment, prob)) {
clock_t end = clock();
stats->sequential_time_sec = (double)(end - start) / CLOCKS_PER_SEC;
return 1;
}
}
clock_t end = clock();
stats->sequential_time_sec = (double)(end - start) / CLOCKS_PER_SEC;
return 0;
}
int solve_sat_membrane(SATProblem *prob, ExecutionStats *stats) {
int num_vars = prob->num_vars;
long long total_membranes = 1LL << num_vars;
clock_t start = clock();
stats->parallel_steps = num_vars;
stats->membranes_created = total_membranes;
int clause_check_steps = (int)ceil(log2(prob->num_clauses + 1));
stats->parallel_steps += clause_check_steps;
stats->parallel_steps += num_vars;
int found_sat = 0;
int assignment[MAX_VARS];
for (long long i = 0; i < total_membranes && !found_sat; i++) {
for (int j = 0; j < num_vars; j++) {
assignment[j] = (i >> j) & 1;
}
if (satisfies_formula(assignment, prob)) {
found_sat = 1;
break;
}
}
clock_t end = clock();
stats->parallel_time_sec = (double)(end - start) / CLOCKS_PER_SEC;
return found_sat;
}
void print_comparison(int num_vars, ExecutionStats *stats) {
printf("\n");
printf("╔════════════════════════════════════════════════════════════════╗\n");
printf("║        Sequential vs Parallel Execution Comparison            ║\n");
printf("╠════════════════════════════════════════════════════════════════╣\n");
printf("║ Problem Size: %2d variables                                    ║\n", num_vars);
printf("╠════════════════════════════════════════════════════════════════╣\n");
printf("║                                                                ║\n");
printf("║ SEQUENTIAL MODEL (Classical Turing Machine)                   ║\n");
printf("║ --------------------------------------------------------       ║\n");
printf("║   Steps taken:        %20lld                  ║\n", stats->sequential_steps);
printf("║   Actual time:        %20.6f seconds         ║\n", stats->sequential_time_sec);
printf("║   Complexity:         O(2^n) = O(2^%d)                        ║\n", num_vars);
printf("║                                                                ║\n");
printf("║ PARALLEL MODEL (Membrane Computing / P-System)                ║\n");
printf("║ --------------------------------------------------------       ║\n");
printf("║   Parallel rounds:    %20lld                  ║\n", stats->parallel_steps);
printf("║   Membranes created:  %20lld                  ║\n", stats->membranes_created);
printf("║   Simulated time:     %20.6f seconds         ║\n", stats->parallel_time_sec);
printf("║   Time complexity:    O(n) = O(%d)                            ║\n", num_vars);
printf("║   Space complexity:   O(2^n) = O(2^%d)                        ║\n", num_vars);
printf("║                                                                ║\n");
printf("║ COMPLEXITY COLLAPSE                                           ║\n");
printf("║ --------------------------------------------------------       ║\n");
double theoretical_speedup = (double)stats->sequential_steps / stats->parallel_steps;
printf("║   Sequential/Parallel steps ratio: %20.2f         ║\n", theoretical_speedup);
printf("║                                                                ║\n");
printf("║   The exponential factor (2^n) moves from TIME to SPACE!      ║\n");
printf("║                                                                ║\n");
printf("╚════════════════════════════════════════════════════════════════╝\n");
}
void demonstrate_matula_parallel() {
printf("\n");
printf("╔════════════════════════════════════════════════════════════════╗\n");
printf("║           Matula Numbers and Parallel Semantics               ║\n");
printf("╠════════════════════════════════════════════════════════════════╣\n");
printf("║                                                                ║\n");
printf("║ Matula Encoding Example: Tree with structure (()()())         ║\n");
printf("║                                                                ║\n");
printf("║   Parentheses:     (()()())                                    ║\n");
printf("║   Interpretation:  Root with 3 children of type ()            ║\n");
printf("║   Matula number:   8 = 2³                                     ║\n");
printf("║   Factorization:   2³ means \"three of type 1\"                 ║\n");
printf("║                                                                ║\n");
printf("║ Computational Interpretation:                                 ║\n");
printf("║ --------------------------------------------------------       ║\n");
printf("║                                                                ║\n");
printf("║   SEQUENTIAL MODEL:                                           ║\n");
printf("║     for i = 1 to 3:                                           ║\n");
printf("║         execute_child()                                       ║\n");
printf("║     Time: 3 sequential steps                                  ║\n");
printf("║                                                                ║\n");
printf("║   PARALLEL MODEL (Membrane):                                  ║\n");
printf("║     execute_all_children_simultaneously()                     ║\n");
printf("║     Time: 1 parallel step                                     ║\n");
printf("║     Space: 3 concurrent membranes                             ║\n");
printf("║                                                                ║\n");
printf("║   The exponent (³) encodes MULTIPLICITY (how many),           ║\n");
printf("║   not DURATION (how long).                                    ║\n");
printf("║                                                                ║\n");
printf("║   Multiplicity = Weight (spatial) ≠ Time (temporal)           ║\n");
printf("║                                                                ║\n");
printf("╚════════════════════════════════════════════════════════════════╝\n");
}
void show_complexity_growth() {
printf("\n");
printf("╔════════════════════════════════════════════════════════════════╗\n");
printf("║             Complexity Growth: Sequential vs Parallel         ║\n");
printf("╠════════════════════════════════════════════════════════════════╣\n");
printf("║  n  │ Sequential O(2^n) │ Parallel O(n) │ Membranes O(2^n)  ║\n");
printf("╠═════╪═══════════════════╪═══════════════╪════════════════════╣\n");
for (int n = 1; n <= 20; n++) {
long long seq_steps = 1LL << n;
long long par_steps = 2 * n;
long long membranes = 1LL << n;
printf("║ %2d  │ %17lld │ %13lld │ %18lld ║\n",
n, seq_steps, par_steps, membranes);
}
printf("╚═════╧═══════════════════╧═══════════════╧════════════════════╝\n");
printf("\n");
printf("Observations:\n");
printf("  • Sequential steps grow exponentially (2^n)\n");
printf("  • Parallel steps grow linearly (2n)\n");
printf("  • Membrane count grows exponentially (2^n)\n");
printf("  • The exponential cost MOVES from TIME to SPACE\n");
printf("  • For n=20: Sequential needs ~1M steps, Parallel needs ~40 steps\n");
printf("  • But: Parallel needs ~1M membranes!\n");
printf("\n");
}
int main(int argc, char **argv) {
int num_vars = 10;
int num_clauses = 30;
if (argc > 1) {
num_vars = atoi(argv[1]);
if (num_vars < 1 || num_vars > MAX_VARS) {
fprintf(stderr, "Error: num_vars must be between 1 and %d\n", MAX_VARS);
return 1;
}
}
if (argc > 2) {
num_clauses = atoi(argv[2]);
if (num_clauses < 1 || num_clauses > MAX_CLAUSES) {
fprintf(stderr, "Error: num_clauses must be between 1 and %d\n", MAX_CLAUSES);
return 1;
}
}
srand(time(NULL));
printf("╔════════════════════════════════════════════════════════════════╗\n");
printf("║  Membrane Computing: P vs NP Complexity Collapse Demo         ║\n");
printf("╠════════════════════════════════════════════════════════════════╣\n");
printf("║                                                                ║\n");
printf("║  This demo shows how membrane computing's maximal parallelism ║\n");
printf("║  collapses the exponential branching of NP problems into      ║\n");
printf("║  polynomial TIME at the cost of exponential SPACE.            ║\n");
printf("║                                                                ║\n");
printf("╚════════════════════════════════════════════════════════════════╝\n");
demonstrate_matula_parallel();
show_complexity_growth();
printf("\n");
printf("═══════════════════════════════════════════════════════════════════\n");
printf(" Running SAT Problem Simulation\n");
printf("═══════════════════════════════════════════════════════════════════\n");
SATProblem prob = generate_sat_problem(num_vars, num_clauses);
ExecutionStats stats = {0};
printf("\nGenerated 3-SAT problem:\n");
printf("  Variables: %d\n", num_vars);
printf("  Clauses: %d\n", num_clauses);
printf("  Total assignments to check: %lld (2^%d)\n",
1LL << num_vars, num_vars);
printf("\nSolving with SEQUENTIAL model...\n");
int seq_result = solve_sat_sequential(&prob, &stats);
printf("  Result: %s\n", seq_result ? "SATISFIABLE" : "UNSATISFIABLE");
printf("\nSolving with PARALLEL (membrane) model...\n");
int par_result = solve_sat_membrane(&prob, &stats);
printf("  Result: %s\n", par_result ? "SATISFIABLE" : "UNSATISFIABLE");
print_comparison(num_vars, &stats);
printf("\n");
printf("═══════════════════════════════════════════════════════════════════\n");
printf(" Key Insights\n");
printf("═══════════════════════════════════════════════════════════════════\n");
printf("\n");
printf("1. CLASSICAL MODEL (Turing Machine):\n");
printf("   • Must check all 2^n assignments SEQUENTIALLY\n");
printf("   • Time complexity: O(2^n)\n");
printf("   • Space complexity: O(n)\n");
printf("   • NP-complete problem: exponential time\n");
printf("\n");
printf("2. MEMBRANE MODEL (P-System):\n");
printf("   • Creates 2^n membranes in n PARALLEL steps\n");
printf("   • Each membrane checks ONE assignment\n");
printf("   • All checks happen SIMULTANEOUSLY\n");
printf("   • Time complexity: O(n) parallel steps\n");
printf("   • Space complexity: O(2^n) membranes\n");
printf("\n");
printf("3. THE COMPLEXITY COLLAPSE:\n");
printf("   • Exponential branching no longer costs exponential TIME\n");
printf("   • Instead, it costs exponential SPACE (membranes)\n");
printf("   • In terms of parallel time: P = NP in membrane model\n");
printf("   • In terms of space: P ≠ NP (still distinct)\n");
printf("\n");
printf("4. MATULA ENCODING CONNECTION:\n");
printf("   • Matula factorization: 8 = 2³\n");
printf("   • Exponent 3 = multiplicity = 3 concurrent copies\n");
printf("   • NOT: 3 sequential time steps\n");
printf("   • Multiplicity is a WEIGHT (spatial), not DURATION (temporal)\n");
printf("\n");
printf("5. PHYSICAL REALITY:\n");
printf("   • Creating 2^n membranes is infeasible for large n\n");
printf("   • For n=10: 1,024 membranes (feasible)\n");
printf("   • For n=30: 1,073,741,824 membranes (infeasible)\n");
printf("   • Theory shows the collapse; practice shows the limits\n");
printf("\n");
printf("═══════════════════════════════════════════════════════════════════\n");
printf("\nUsage: %s [num_vars] [num_clauses]\n", argv[0]);
printf("  num_vars: Number of Boolean variables (1-%d, default: 10)\n", MAX_VARS);
printf("  num_clauses: Number of clauses (1-%d, default: 30)\n", MAX_CLAUSES);
printf("\n");
return 0;
}