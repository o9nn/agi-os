import { Streamdown } from "streamdown";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Network, GitBranch, RefreshCw } from "lucide-react";
import A000081Viz from "@/components/A000081Viz";

const content = `
# Ontogenetic Engine

The **Ontogenetic Engine** is the central nervous system of Deep Tree Echo, unifying all components under the guidance of the OEIS A000081 sequence. It orchestrates the co-evolution of structure and dynamics.

## A000081 Guidance

The sequence A000081 (1, 1, 2, 4, 9, 20, 48, 115...) represents the number of rooted trees with $n$ nodes. This sequence dictates the fundamental parameters of the system:

- **Reservoir Size**: $\\sum A000081[1:n]$
- **Growth Rate**: $A000081[n+1] / A000081[n]$
- **Mutation Rate**: $1 / A000081[n]$

## Unified Dynamics

The engine solves the master equation:

$$
\\frac{\\partial \\Psi}{\\partial t} = J_{A000081}(\\Psi) \\cdot \\nabla H_{A000081}(\\Psi) + R_{\\text{echo}}(\\Psi, T) + M_{\\text{membrane}}(\\Psi, T) + F_{\\text{feedback}}(\\Psi, T)
$$

This equation ensures that the system evolves in a way that is mathematically consistent with the topology of rooted trees, leading to emergent self-organization.

## Feedback Loops

The engine maintains bidirectional feedback loops between all components:
- **Tree $\\leftrightarrow$ Membrane**: Trees structure membranes; membranes evolve trees.
- **J-Surface $\\leftrightarrow$ Reservoir**: Surface gradients drive reservoir states; reservoirs deform the surface.
- **Global $\\leftrightarrow$ Local**: System-wide coherence emerges from local interactions.
`;

export default function OntogeneticEngine() {
  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="relative rounded-3xl overflow-hidden border border-border/50 bg-card/30 backdrop-blur-md">
        <div className="absolute inset-0 bg-[url('/images/ontogenetic-engine.jpg')] bg-cover bg-center opacity-20 mix-blend-screen" />
        <div className="absolute inset-0 bg-gradient-to-r from-background via-background/80 to-transparent" />
        
        <div className="relative p-8 md:p-12 lg:p-16 max-w-3xl">
          <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-chart-3/10 text-chart-3 text-sm font-medium mb-6">
            <Network className="h-4 w-4" />
            <span>Central Nervous System</span>
          </div>
          <h1 className="text-4xl md:text-5xl lg:text-6xl font-heading font-bold mb-6 tracking-tight">
            Ontogenetic Engine
          </h1>
          <p className="text-lg md:text-xl text-muted-foreground leading-relaxed">
            The unified feedback system guided by OEIS A000081, orchestrating the co-evolution of structure and dynamics.
          </p>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
        <div className="lg:col-span-2 space-y-8">
          <div className="prose prose-invert prose-headings:font-heading prose-headings:tracking-tight prose-p:text-muted-foreground prose-a:text-primary prose-code:text-primary prose-pre:bg-card/50 prose-pre:border prose-pre:border-border/50 max-w-none">
            <Streamdown>{content}</Streamdown>
          </div>
          <A000081Viz />
        </div>

        <div className="space-y-6">
          <Card className="border-border/50 bg-card/50 backdrop-blur-sm">
            <CardHeader>
              <CardTitle className="font-heading text-lg flex items-center gap-2">
                <GitBranch className="h-5 w-5 text-chart-3" />
                A000081 Sequence
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="flex flex-wrap gap-2">
                {[1, 1, 2, 4, 9, 20, 48, 115].map((num, i) => (
                  <div key={i} className="flex flex-col items-center p-2 bg-background/50 rounded border border-border/50 min-w-[3rem]">
                    <span className="text-xs text-muted-foreground">n={i+1}</span>
                    <span className="font-mono font-bold text-chart-3">{num}</span>
                  </div>
                ))}
                <div className="flex items-center justify-center p-2 text-muted-foreground">...</div>
              </div>
            </CardContent>
          </Card>

          <Card className="border-border/50 bg-card/50 backdrop-blur-sm">
            <CardHeader>
              <CardTitle className="font-heading text-lg flex items-center gap-2">
                <RefreshCw className="h-5 w-5 text-primary" />
                Cycle Status
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Generation</span>
                  <span className="font-mono">50</span>
                </div>
                <div className="w-full bg-secondary h-2 rounded-full overflow-hidden">
                  <div className="bg-primary h-full w-[80%]" />
                </div>
              </div>
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Coherence</span>
                  <span className="font-mono">0.98</span>
                </div>
                <div className="w-full bg-secondary h-2 rounded-full overflow-hidden">
                  <div className="bg-chart-2 h-full w-[98%]" />
                </div>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}
