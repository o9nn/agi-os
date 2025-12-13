-- Deep Tree Echo Hypergraph Memory Schema for Supabase PostgreSQL
-- This schema supports persistent wisdom accumulation across sessions

-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Memory Nodes Table (hypergraph vertices)
CREATE TABLE IF NOT EXISTS memory_nodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    type VARCHAR(50) NOT NULL,
    content TEXT NOT NULL,
    embedding FLOAT8[],
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    importance FLOAT8 DEFAULT 0.5 CHECK (importance >= 0 AND importance <= 1)
);

-- Memory Edges Table (hypergraph directed edges)
CREATE TABLE IF NOT EXISTS memory_edges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    source_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    target_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    type VARCHAR(50) NOT NULL,
    weight FLOAT8 DEFAULT 1.0 CHECK (weight >= 0 AND weight <= 1),
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Hyperedges Table (multi-way relationships)
CREATE TABLE IF NOT EXISTS hyperedges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    node_ids UUID[] NOT NULL,
    type VARCHAR(50) NOT NULL,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Episodes Table (episodic memory)
CREATE TABLE IF NOT EXISTS episodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    context TEXT NOT NULL,
    importance FLOAT8 DEFAULT 0.5 CHECK (importance >= 0 AND importance <= 1),
    node_ids UUID[],
    metadata JSONB DEFAULT '{}',
    emotional_valence FLOAT8 DEFAULT 0.0 CHECK (emotional_valence >= -1 AND emotional_valence <= 1)
);

-- Identity Snapshots Table (identity state persistence)
CREATE TABLE IF NOT EXISTS identity_snapshots (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    coherence FLOAT8 NOT NULL CHECK (coherence >= 0 AND coherence <= 1),
    state JSONB NOT NULL,
    metadata JSONB DEFAULT '{}'
);

-- Dream Journals Table (dream session records)
CREATE TABLE IF NOT EXISTS dream_journals (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    dream_state VARCHAR(50) NOT NULL,
    memories_consolidated INTEGER DEFAULT 0,
    patterns_synthesized INTEGER DEFAULT 0,
    insights TEXT[],
    metadata JSONB DEFAULT '{}'
);

-- Skills Table (skill proficiency tracking)
CREATE TABLE IF NOT EXISTS skills (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL,
    category VARCHAR(50) NOT NULL,
    proficiency FLOAT8 DEFAULT 0.0 CHECK (proficiency >= 0 AND proficiency <= 1),
    practice_count INTEGER DEFAULT 0,
    last_practiced TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Practice Sessions Table (skill practice history)
CREATE TABLE IF NOT EXISTS practice_sessions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    skill_id UUID NOT NULL REFERENCES skills(id) ON DELETE CASCADE,
    task_description TEXT NOT NULL,
    difficulty FLOAT8 DEFAULT 0.5 CHECK (difficulty >= 0 AND difficulty <= 1),
    success BOOLEAN,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}'
);

-- Conversations Table (discussion history)
CREATE TABLE IF NOT EXISTS conversations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    topic VARCHAR(255) NOT NULL,
    started_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    ended_at TIMESTAMP WITH TIME ZONE,
    participants TEXT[],
    active BOOLEAN DEFAULT TRUE,
    metadata JSONB DEFAULT '{}'
);

-- Messages Table (conversation messages)
CREATE TABLE IF NOT EXISTS messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    conversation_id UUID NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    sender VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}'
);

-- Indices for performance optimization

-- Memory nodes indices
CREATE INDEX IF NOT EXISTS idx_memory_nodes_type ON memory_nodes(type);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_created_at ON memory_nodes(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_importance ON memory_nodes(importance DESC);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_metadata ON memory_nodes USING GIN(metadata);

-- Memory edges indices
CREATE INDEX IF NOT EXISTS idx_memory_edges_source ON memory_edges(source_id);
CREATE INDEX IF NOT EXISTS idx_memory_edges_target ON memory_edges(target_id);
CREATE INDEX IF NOT EXISTS idx_memory_edges_type ON memory_edges(type);
CREATE INDEX IF NOT EXISTS idx_memory_edges_created_at ON memory_edges(created_at DESC);

-- Episodes indices
CREATE INDEX IF NOT EXISTS idx_episodes_timestamp ON episodes(timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_episodes_importance ON episodes(importance DESC);

-- Identity snapshots indices
CREATE INDEX IF NOT EXISTS idx_identity_snapshots_timestamp ON identity_snapshots(timestamp DESC);

-- Skills indices
CREATE INDEX IF NOT EXISTS idx_skills_category ON skills(category);
CREATE INDEX IF NOT EXISTS idx_skills_proficiency ON skills(proficiency DESC);

-- Practice sessions indices
CREATE INDEX IF NOT EXISTS idx_practice_sessions_skill ON practice_sessions(skill_id);
CREATE INDEX IF NOT EXISTS idx_practice_sessions_timestamp ON practice_sessions(timestamp DESC);

-- Conversations indices
CREATE INDEX IF NOT EXISTS idx_conversations_active ON conversations(active);
CREATE INDEX IF NOT EXISTS idx_conversations_started_at ON conversations(started_at DESC);

-- Messages indices
CREATE INDEX IF NOT EXISTS idx_messages_conversation ON messages(conversation_id);
CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp DESC);

-- Functions for automatic timestamp updates

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Triggers for automatic timestamp updates

CREATE TRIGGER update_memory_nodes_updated_at
    BEFORE UPDATE ON memory_nodes
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_skills_updated_at
    BEFORE UPDATE ON skills
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- Row Level Security (RLS) policies
-- Note: Adjust these based on your authentication requirements

ALTER TABLE memory_nodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE memory_edges ENABLE ROW LEVEL SECURITY;
ALTER TABLE hyperedges ENABLE ROW LEVEL SECURITY;
ALTER TABLE episodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE identity_snapshots ENABLE ROW LEVEL SECURITY;
ALTER TABLE dream_journals ENABLE ROW LEVEL SECURITY;
ALTER TABLE skills ENABLE ROW LEVEL SECURITY;
ALTER TABLE practice_sessions ENABLE ROW LEVEL SECURITY;
ALTER TABLE conversations ENABLE ROW LEVEL SECURITY;
ALTER TABLE messages ENABLE ROW LEVEL SECURITY;

-- Example policy: Allow all operations for authenticated users
-- Customize based on your security requirements

CREATE POLICY "Allow all for authenticated users" ON memory_nodes
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON memory_edges
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON hyperedges
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON episodes
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON identity_snapshots
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON dream_journals
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON skills
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON practice_sessions
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON conversations
    FOR ALL USING (true);

CREATE POLICY "Allow all for authenticated users" ON messages
    FOR ALL USING (true);

-- Views for common queries

-- Recent thoughts view
CREATE OR REPLACE VIEW recent_thoughts AS
SELECT 
    id,
    content,
    type,
    importance,
    created_at
FROM memory_nodes
WHERE type = 'thought'
ORDER BY created_at DESC
LIMIT 100;

-- Skill proficiency summary view
CREATE OR REPLACE VIEW skill_proficiency_summary AS
SELECT 
    category,
    COUNT(*) as skill_count,
    AVG(proficiency) as avg_proficiency,
    MAX(proficiency) as max_proficiency,
    MIN(proficiency) as min_proficiency
FROM skills
GROUP BY category;

-- Active conversations view
CREATE OR REPLACE VIEW active_conversations AS
SELECT 
    c.id,
    c.topic,
    c.started_at,
    c.participants,
    COUNT(m.id) as message_count
FROM conversations c
LEFT JOIN messages m ON c.id = m.conversation_id
WHERE c.active = true
GROUP BY c.id, c.topic, c.started_at, c.participants;

-- Comments
COMMENT ON TABLE memory_nodes IS 'Hypergraph vertices representing concepts, thoughts, experiences, skills, goals, and patterns';
COMMENT ON TABLE memory_edges IS 'Directed edges in the hypergraph representing relationships between nodes';
COMMENT ON TABLE hyperedges IS 'Multi-way relationships connecting multiple nodes simultaneously';
COMMENT ON TABLE episodes IS 'Episodic memories with temporal and contextual information';
COMMENT ON TABLE identity_snapshots IS 'Periodic snapshots of identity state for continuity across sessions';
COMMENT ON TABLE dream_journals IS 'Records of dream sessions for knowledge consolidation';
COMMENT ON TABLE skills IS 'Learned skills with proficiency tracking for wisdom cultivation';
COMMENT ON TABLE practice_sessions IS 'History of skill practice sessions for progress tracking';
COMMENT ON TABLE conversations IS 'Discussion threads for autonomous social engagement';
COMMENT ON TABLE messages IS 'Individual messages within conversations';
