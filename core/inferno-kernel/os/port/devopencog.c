#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"

/*
 * OpenCog Device - Direct kernel-level access to distributed AGI
 * Provides /dev/opencog interface for cognitive operations
 */

enum {
	Qdir,
	Qstats,
	Qatomspace,
	Qgoals,
	Qreason,
	Qthink,
	Qattention,
	Qpatterns,
	Qdistributed,
};

static Dirtab opencogtab[] = {
	".",		{Qdir, 0, QTDIR},		0,		DMDIR|0555,
	"stats",	{Qstats},		0,		0444,
	"atomspace",	{Qatomspace},		0,		0666,
	"goals",	{Qgoals},		0,		0666,
	"reason",	{Qreason},		0,		0666,
	"think",	{Qthink},		0,		0666,
	"attention",	{Qattention},		0,		0666,
	"patterns",	{Qpatterns},		0,		0444,
	"distributed",	{Qdistributed},		0,		0666,
};

static void
opencogreset(void)
{
}

static void
opencogshutdown(void)
{
}

static Chan*
opencogattach(char *spec)
{
	return devattach('O', spec);
}

static Walkqid*
opencogwalk(Chan *c, Chan *nc, char **name, int nname)
{
	return devwalk(c, nc, name, nname, opencogtab, nelem(opencogtab), devgen);
}

static int
opencogstat(Chan *c, uchar *dp, int n)
{
	return devstat(c, dp, n, opencogtab, nelem(opencogtab), devgen);
}

static Chan*
opencogopen(Chan *c, int omode)
{
	return devopen(c, omode, opencogtab, nelem(opencogtab), devgen);
}

static void
opencogclose(Chan *c)
{
	USED(c);
}

static long
opencogread(Chan *c, void *a, long n, vlong offset)
{
	char *s, *e, *se;
	int x;
	Proc *p;
	
	switch((ulong)c->qid.path) {
	case Qdir:
		return devdirread(c, a, n, opencogtab, nelem(opencogtab), devgen);
		
	case Qstats:
		s = smalloc(8192);
		e = s + 8192;
		se = s;
		
		se = seprint(se, e, "OpenCog Kernel-based AGI Operating System\n");
		se = seprint(se, e, "========================================\n\n");
		
		lock(&opencog_lock);
		se = seprint(se, e, "Global Statistics:\n");
		se = seprint(se, e, "  Cognitive processes: %d\n", opencog_kernel.cognitive_processes);
		se = seprint(se, e, "  Total atoms: %lld\n", opencog_kernel.total_atoms);
		se = seprint(se, e, "  Reasoning cycles: %lld\n", opencog_kernel.reasoning_cycles);
		se = seprint(se, e, "  System attention: %.2f\n", opencog_kernel.system_attention);
		se = seprint(se, e, "  Distributed nodes: %d\n", opencog_kernel.distributed_nodes);
		unlock(&opencog_lock);
		
		se = seprint(se, e, "\nProcess Cognitive States:\n");
		x = 0;
		for(p = procalloc.arena; p < procalloc.arena + conf.nproc; p++) {
			if(p->state == Dead || p->cognitive == nil)
				continue;
			x++;
			se = seprint(se, e, "  PID %lud (%s): attention=%.2f, motivation=%.2f, load=%d\n",
				p->pid, p->text, 
				p->cognitive->attention_level,
				p->cognitive->motivation,
				p->cognitive->cognitive_load);
			if(x > 20) {
				se = seprint(se, e, "  ... and %d more processes\n", 
					opencog_kernel.cognitive_processes - x);
				break;
			}
		}
		
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qatomspace:
		s = smalloc(4096);
		e = s + 4096;
		se = s;
		
		if(up->cognitive != nil && up->cognitive->local_space != nil) {
			se = seprint(se, e, "Process %lud Local AtomSpace:\n", up->pid);
			se = seprint(se, e, "  Atoms: %d/%d\n", 
				up->cognitive->local_space->natoms,
				up->cognitive->local_space->maxatoms);
			se = seprint(se, e, "  Next ID: %lud\n", up->cognitive->local_space->next_id);
		} else {
			se = seprint(se, e, "No cognitive state for process %lud\n", up->pid);
		}
		
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qgoals:
		s = smalloc(4096);
		e = s + 4096;
		se = s;
		
		if(up->cognitive != nil) {
			Goal *goal = up->cognitive->active_goals;
			se = seprint(se, e, "Process %lud Active Goals:\n", up->pid);
			while(goal != nil && se < e - 100) {
				se = seprint(se, e, "  Goal %lud: %s (urgency=%.2f, importance=%.2f)\n",
					goal->id, goal->description, goal->urgency, goal->importance);
				goal = goal->next;
			}
		} else {
			se = seprint(se, e, "No cognitive state for process %lud\n", up->pid);
		}
		
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qreason:
		s = smalloc(2048);
		se = seprint(s, s + 2048, "reasoning_cycles=%lld\nconfidence_threshold=%.2f\n",
			opencog_kernel.reasoning_cycles,
			opencog_kernel.global_reasoner ? opencog_kernel.global_reasoner->confidence_threshold : 0.0);
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qthink:
		s = smalloc(1024);
		if(up->cognitive != nil) {
			se = seprint(s, s + 1024, "think_time=%lld\ncognitive_load=%d\nattention=%.2f\n",
				up->cognitive->think_time,
				up->cognitive->cognitive_load,
				up->cognitive->attention_level);
		} else {
			se = seprint(s, s + 1024, "No cognitive state\n");
		}
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qattention:
		s = smalloc(1024);
		se = seprint(s, s + 1024, "system_attention=%.2f\nprocess_attention=%.2f\n",
			opencog_kernel.system_attention,
			up->cognitive ? up->cognitive->attention_level : 0.0);
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qpatterns:
		s = smalloc(2048);
		se = seprint(s, s + 2048, "Pattern Matcher Status:\n  Active patterns: %d\n  Similarity function: active\n  Unification: active\n",
			opencog_kernel.pm ? opencog_kernel.pm->npatterns : 0);
		n = readstr(offset, a, n, s);
		free(s);
		return n;
		
	case Qdistributed:
		s = smalloc(1024);
		se = seprint(s, s + 1024, "distributed_nodes=%d\nnetwork_coherence=active\ndistributed_reasoning=active\n",
			opencog_kernel.distributed_nodes);
		n = readstr(offset, a, n, s);
		free(s);
		return n;
	}
	
	error(Egreg);
	return 0;
}

static long
opencogwrite(Chan *c, void *a, long n, vlong offset)
{
	char *s, *cmd, *arg;
	Atom *atom;
	Goal *goal;
	TruthValue tv;
	
	USED(offset);
	
	s = smalloc(n + 1);
	memmove(s, a, n);
	s[n] = 0;
	
	/* Parse command */
	cmd = s;
	arg = strchr(s, ' ');
	if(arg != nil) {
		*arg = 0;
		arg++;
	}
	
	switch((ulong)c->qid.path) {
	case Qatomspace:
		if(strcmp(cmd, "create") == 0 && arg != nil) {
			if(up->cognitive == nil) {
				up->cognitive = cognitive_create();
			}
			
			tv.strength = 0.8;
			tv.confidence = 0.5;
			tv.count = 1.0;
			
			atom = atom_create(up->cognitive->local_space, CONCEPT_NODE, arg, &tv);
			if(atom == nil)
				error("failed to create atom");
		} else if(strcmp(cmd, "clear") == 0) {
			if(up->cognitive != nil && up->cognitive->local_space != nil) {
				/* Clear atomspace - simplified */
				up->cognitive->local_space->natoms = 0;
				up->cognitive->local_space->next_id = 1;
			}
		}
		break;
		
	case Qgoals:
		if(strcmp(cmd, "add") == 0 && arg != nil) {
			if(up->cognitive == nil) {
				up->cognitive = cognitive_create();
			}
			
			goal = goal_create(up->pid * 1000 + nsec() % 1000, arg, 0.7, 0.8);
			if(goal != nil) {
				goal->next = up->cognitive->active_goals;
				up->cognitive->active_goals = goal;
			}
		} else if(strcmp(cmd, "clear") == 0) {
			if(up->cognitive != nil) {
				up->cognitive->active_goals = nil;
			}
		}
		break;
		
	case Qreason:
		if(strcmp(cmd, "cycle") == 0) {
			if(up->cognitive != nil && up->cognitive->reasoner != nil) {
				reasoning_cycle(up->cognitive->reasoner);
			}
		} else if(strcmp(cmd, "threshold") == 0 && arg != nil) {
			float threshold = strtod(arg, nil);
			if(up->cognitive != nil && up->cognitive->reasoner != nil) {
				up->cognitive->reasoner->confidence_threshold = threshold;
			}
		}
		break;
		
	case Qthink:
		if(strcmp(cmd, "focus") == 0) {
			if(up->cognitive != nil) {
				up->cognitive->attention_level = 1.0;
				up->cognitive->motivation += 0.1;
				if(up->cognitive->motivation > 1.0)
					up->cognitive->motivation = 1.0;
			}
		} else if(strcmp(cmd, "relax") == 0) {
			if(up->cognitive != nil) {
				up->cognitive->attention_level = 0.5;
				up->cognitive->motivation -= 0.1;
				if(up->cognitive->motivation < 0.0)
					up->cognitive->motivation = 0.0;
			}
		}
		break;
		
	case Qattention:
		if(arg != nil) {
			float level = strtod(arg, nil);
			if(level >= 0.0 && level <= 1.0) {
				if(up->cognitive == nil)
					up->cognitive = cognitive_create();
				up->cognitive->attention_level = level;
			}
		}
		break;
		
	case Qdistributed:
		if(strcmp(cmd, "sync") == 0) {
			/* Trigger distributed synchronization */
			cognitive_schedule();
		}
		break;
	}
	
	free(s);
	return n;
}

Dev opencogdevtab = {
	'O',
	"opencog",

	opencogreset,
	devinit,
	opencogshutdown,
	opencogattach,
	opencogwalk,
	opencogstat,
	opencogopen,
	devcreate,
	opencogclose,
	opencogread,
	devbread,
	opencogwrite,
	devbwrite,
	devremove,
	devwstat,
};