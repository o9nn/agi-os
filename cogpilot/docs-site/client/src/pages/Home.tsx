import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { ArrowRight, Activity, Layers, Network, Zap, GitBranch, Database } from "lucide-react";
import { Link } from "wouter";

export default function Home() {
  return (
    <div className="flex flex-col gap-16 md:gap-24">
      {/* Hero Section */}
      <section className="relative flex flex-col items-center justify-center text-center gap-8 py-12 md:py-20 lg:py-32 overflow-hidden">
        <div className="absolute inset-0 -z-10">
          <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[800px] h-[800px] bg-primary/10 rounded-full blur-[120px] animate-pulse-slow" />
          <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[600px] h-[600px] bg-chart-2/10 rounded-full blur-[100px] animate-pulse-slower delay-1000" />
        </div>
        
        <div className="space-y-4 max-w-4xl mx-auto animate-in fade-in slide-in-from-bottom-8 duration-700">
          <Badge variant="outline" className="px-4 py-1.5 rounded-full border-primary/30 bg-primary/5 text-primary hover:bg-primary/10 transition-colors cursor-default">
            <span className="mr-2 h-2 w-2 rounded-full bg-primary animate-pulse" />
            System Evolution Complete
          </Badge>
          <h1 className="text-4xl md:text-6xl lg:text-7xl font-heading font-bold tracking-tight text-transparent bg-clip-text bg-gradient-to-b from-foreground to-foreground/50 pb-2">
            Deep Tree Echo
            <span className="block text-2xl md:text-3xl lg:text-4xl font-normal text-muted-foreground mt-4">
              State Reservoir Computer
            </span>
          </h1>
          <p className="text-lg md:text-xl text-muted-foreground max-w-2xl mx-auto leading-relaxed">
            A unified system integrating J-Surface elementary differentials, B-series ridges, and P-system reservoirs under the ontogenetic guidance of OEIS A000081.
          </p>
        </div>

        <div className="flex flex-wrap items-center justify-center gap-4 animate-in fade-in slide-in-from-bottom-8 duration-700 delay-200">
          <Link href="/evolution">
            <Button size="lg" className="h-12 px-8 text-base rounded-full bg-primary hover:bg-primary/90 text-primary-foreground shadow-[0_0_20px_-5px_var(--primary)] hover:shadow-[0_0_30px_-5px_var(--primary)] transition-all duration-300">
              Explore Evolution
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </Link>
          <Link href="/j-surface">
            <Button variant="outline" size="lg" className="h-12 px-8 text-base rounded-full border-primary/20 hover:bg-primary/5 hover:text-primary hover:border-primary/50 transition-all duration-300">
              View Architecture
            </Button>
          </Link>
        </div>
      </section>

      {/* Core Components Grid */}
      <section className="grid grid-cols-1 md:grid-cols-3 gap-6 lg:gap-8">
        <Link href="/j-surface">
          <Card className="group relative overflow-hidden border-border/50 bg-card/50 backdrop-blur-sm hover:bg-card/80 hover:border-primary/30 transition-all duration-500 cursor-pointer h-full">
            <div className="absolute inset-0 bg-gradient-to-br from-primary/5 via-transparent to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-500" />
            <CardHeader>
              <div className="mb-4 inline-flex h-12 w-12 items-center justify-center rounded-lg bg-primary/10 text-primary ring-1 ring-primary/20 group-hover:scale-110 transition-transform duration-500">
                <Layers className="h-6 w-6" />
              </div>
              <CardTitle className="font-heading text-xl group-hover:text-primary transition-colors">J-Surface Reactor</CardTitle>
              <CardDescription>Elementary differentials & B-series ridges</CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground text-sm leading-relaxed">
                Unites gradient flow with B-series integration on a symplectic manifold, creating a unified landscape for energy minimization and dynamic evolution.
              </p>
            </CardContent>
          </Card>
        </Link>

        <Link href="/membrane-gardens">
          <Card className="group relative overflow-hidden border-border/50 bg-card/50 backdrop-blur-sm hover:bg-card/80 hover:border-chart-2/30 transition-all duration-500 cursor-pointer h-full">
            <div className="absolute inset-0 bg-gradient-to-br from-chart-2/5 via-transparent to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-500" />
            <CardHeader>
              <div className="mb-4 inline-flex h-12 w-12 items-center justify-center rounded-lg bg-chart-2/10 text-chart-2 ring-1 ring-chart-2/20 group-hover:scale-110 transition-transform duration-500">
                <Activity className="h-6 w-6" />
              </div>
              <CardTitle className="font-heading text-xl group-hover:text-chart-2 transition-colors">Membrane Gardens</CardTitle>
              <CardDescription>P-System reservoirs & rooted trees</CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground text-sm leading-relaxed">
                Hierarchical membrane structures cultivating rooted trees, where reservoir states couple with J-surface dynamics to drive evolutionary feedback.
              </p>
            </CardContent>
          </Card>
        </Link>

        <Link href="/ontogenetic-engine">
          <Card className="group relative overflow-hidden border-border/50 bg-card/50 backdrop-blur-sm hover:bg-card/80 hover:border-chart-3/30 transition-all duration-500 cursor-pointer h-full">
            <div className="absolute inset-0 bg-gradient-to-br from-chart-3/5 via-transparent to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-500" />
            <CardHeader>
              <div className="mb-4 inline-flex h-12 w-12 items-center justify-center rounded-lg bg-chart-3/10 text-chart-3 ring-1 ring-chart-3/20 group-hover:scale-110 transition-transform duration-500">
                <Network className="h-6 w-6" />
              </div>
              <CardTitle className="font-heading text-xl group-hover:text-chart-3 transition-colors">Ontogenetic Engine</CardTitle>
              <CardDescription>A000081-guided unified feedback</CardDescription>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground text-sm leading-relaxed">
                The central nervous system integrating all components under the OEIS A000081 sequence, driving self-organization and emergent behavior.
              </p>
            </CardContent>
          </Card>
        </Link>
      </section>

      {/* Feature Highlights */}
      <section className="relative rounded-3xl border border-border/50 bg-card/30 backdrop-blur-md overflow-hidden">
        <div className="absolute inset-0 bg-[url('/images/j-surface-reactor.jpg')] bg-cover bg-center opacity-10 mix-blend-overlay" />
        <div className="relative grid grid-cols-1 lg:grid-cols-2 gap-12 p-8 md:p-12 lg:p-16 items-center">
          <div className="space-y-8">
            <div className="space-y-4">
              <h2 className="text-3xl md:text-4xl font-heading font-bold">Mathematical Unification</h2>
              <p className="text-lg text-muted-foreground leading-relaxed">
                The system achieves true cohesion through a master equation that combines continuous gradient flow, discrete B-series integration, and evolutionary computation.
              </p>
            </div>
            
            <div className="space-y-6">
              <div className="flex gap-4">
                <div className="flex-none mt-1">
                  <div className="h-8 w-8 rounded-full bg-primary/10 flex items-center justify-center text-primary">
                    <Zap className="h-4 w-4" />
                  </div>
                </div>
                <div>
                  <h3 className="font-heading font-semibold text-lg mb-2">Symplectic Structure</h3>
                  <p className="text-muted-foreground text-sm">Preserves energy conservation and phase space volume through J-surface geometry.</p>
                </div>
              </div>
              
              <div className="flex gap-4">
                <div className="flex-none mt-1">
                  <div className="h-8 w-8 rounded-full bg-chart-2/10 flex items-center justify-center text-chart-2">
                    <GitBranch className="h-4 w-4" />
                  </div>
                </div>
                <div>
                  <h3 className="font-heading font-semibold text-lg mb-2">B-Series Integration</h3>
                  <p className="text-muted-foreground text-sm">Uses elementary differentials for rooted trees to bridge discrete and continuous dynamics.</p>
                </div>
              </div>
              
              <div className="flex gap-4">
                <div className="flex-none mt-1">
                  <div className="h-8 w-8 rounded-full bg-chart-3/10 flex items-center justify-center text-chart-3">
                    <Database className="h-4 w-4" />
                  </div>
                </div>
                <div>
                  <h3 className="font-heading font-semibold text-lg mb-2">A000081 Parameters</h3>
                  <p className="text-muted-foreground text-sm">All system parameters are derived from the OEIS A000081 sequence for ontogenetic consistency.</p>
                </div>
              </div>
            </div>
          </div>
          
          <div className="relative aspect-square lg:aspect-auto lg:h-full min-h-[400px] rounded-2xl overflow-hidden border border-border/50 shadow-2xl">
            <img 
              src="/images/ontogenetic-engine.jpg" 
              alt="Ontogenetic Engine Visualization" 
              className="absolute inset-0 w-full h-full object-cover hover:scale-105 transition-transform duration-700"
            />
            <div className="absolute inset-0 bg-gradient-to-t from-background/80 via-transparent to-transparent" />
            <div className="absolute bottom-0 left-0 p-6">
              <div className="font-mono text-xs text-primary mb-2">OEIS A000081</div>
              <div className="font-heading font-bold text-xl">Self-Organizing Evolution</div>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}
