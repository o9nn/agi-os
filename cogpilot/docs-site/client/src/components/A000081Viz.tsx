import { useEffect, useRef } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Network } from "lucide-react";

const SEQUENCE = [1, 1, 2, 4, 9, 20, 48, 115];

export default function A000081Viz() {
  const canvasRef = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    let animationFrameId: number;
    let time = 0;

    const resize = () => {
      const parent = canvas.parentElement;
      if (parent) {
        canvas.width = parent.clientWidth;
        canvas.height = 300;
      }
    };

    window.addEventListener("resize", resize);
    resize();

    const draw = () => {
      time += 0.01;
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      const centerX = canvas.width / 2;
      const centerY = canvas.height / 2;
      
      // Draw concentric rings representing the sequence
      SEQUENCE.forEach((count, i) => {
        const radius = 20 + i * 15;
        const alpha = 0.1 + (Math.sin(time + i * 0.5) + 1) * 0.1;
        
        ctx.beginPath();
        ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
        ctx.strokeStyle = `oklch(0.7 0.15 160 / ${alpha})`;
        ctx.lineWidth = 1;
        ctx.stroke();

        // Draw orbiting particles
        const particles = Math.min(count, 20); // Limit particles for performance
        for (let j = 0; j < particles; j++) {
          const angle = (time * (1 + i * 0.1) + (j / particles) * Math.PI * 2);
          const x = centerX + Math.cos(angle) * radius;
          const y = centerY + Math.sin(angle) * radius;
          
          ctx.beginPath();
          ctx.arc(x, y, 2, 0, Math.PI * 2);
          ctx.fillStyle = `oklch(0.7 0.15 160 / 0.5)`;
          ctx.fill();
        }
      });

      animationFrameId = requestAnimationFrame(draw);
    };

    draw();

    return () => {
      window.removeEventListener("resize", resize);
      cancelAnimationFrame(animationFrameId);
    };
  }, []);

  return (
    <Card className="border-border/50 bg-card/50 backdrop-blur-sm overflow-hidden">
      <CardHeader>
        <CardTitle className="font-heading text-lg flex items-center gap-2">
          <Network className="h-5 w-5 text-primary" />
          Ontogenetic Resonance
        </CardTitle>
      </CardHeader>
      <CardContent className="p-0">
        <canvas ref={canvasRef} className="w-full h-[300px]" />
      </CardContent>
    </Card>
  );
}
