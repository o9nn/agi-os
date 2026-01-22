import OpenAI from "openai";
import { supabase } from "./supabaseClient";
import {
  Mem0ry,
  Mem0ryType,
  Mem0rySearchResult,
  Mem0ryQueryOptions,
  Mem0ryStats,
  Mem0AISummary,
} from "../types/Mem0AI";
class Mem0AIService {
  private static instance: Mem0AIService;
  private openai: OpenAI | null = null;
  private userId: string | null = null;
  private constructor() {}
  public static getInstance(): Mem0AIService {
    if (!Mem0AIService.instance) {
      Mem0AIService.instance = new Mem0AIService();
    }
    return Mem0AIService.instance;
  }
  public initialize(openAIKey: string, userId: string): void {
    this.openai = new OpenAI({
      apiKey: openAIKey,
      dangerouslyAllowBrowser: true,
    });
    this.userId = userId;
  }
  public isInitialized(): boolean {
    return !!this.openai && !!this.userId;
  }
  private async generateEmbedding(text: string): Promise<number[]> {
    if (!this.openai) {
      throw new Error("OpenAI client not initialized");
    }
    try {
      const response = await this.openai.embeddings.create({
        model: "text-embedding-3-large",
        input: text,
        dimensions: 1536,
      });
      return response.data[0].embedding;
    } catch (error) {
      console.error("Error generating embedding:", error);
      throw error;
    }
  }
  public async addMemory(
    memory: Omit<Mem0ry, "id" | "createdAt" | "updatedAt" | "embedding">
  ): Promise<Mem0ry> {
    if (!this.openai || !this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const embedding = await this.generateEmbedding(memory.content);
      const now = new Date().toISOString();
      const { data, error } = await supabase
        .from("memories")
        .insert({
          user_id: this.userId,
          title: memory.title,
          content: memory.content,
          tags: memory.tags || [],
          embedding,
          created_at: now,
          updated_at: now,
          metadata: memory.metadata || {},
          context: memory.context || null,
          type: memory.type || "memory",
        })
        .select("id")
        .single();
      if (error) {
        throw error;
      }
      return {
        id: data.id,
        title: memory.title,
        content: memory.content,
        tags: memory.tags || [],
        createdAt: now,
        updatedAt: now,
        metadata: memory.metadata,
        context: memory.context,
        type: memory.type,
        embedding,
      };
    } catch (error) {
      console.error("Error adding memory:", error);
      throw error;
    }
  }
  public async updateMemory(
    id: string,
    updates: Partial<
      Omit<Mem0ry, "id" | "createdAt" | "updatedAt" | "embedding">
    >
  ): Promise<Mem0ry> {
    if (!this.openai || !this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      let embedding = undefined;
      if (updates.content) {
        embedding = await this.generateEmbedding(updates.content);
      }
      const now = new Date().toISOString();
      const updateData: any = {
        ...updates,
        updated_at: now,
      };
      if (embedding) {
        updateData.embedding = embedding;
      }
      const { data, error } = await supabase
        .from("memories")
        .update(updateData)
        .eq("id", id)
        .eq("user_id", this.userId)
        .select("*")
        .single();
      if (error) {
        throw error;
      }
      return {
        id: data.id,
        title: data.title,
        content: data.content,
        tags: data.tags,
        createdAt: data.created_at,
        updatedAt: data.updated_at,
        metadata: data.metadata,
        context: data.context,
        type: data.type,
        embedding: data.embedding,
      };
    } catch (error) {
      console.error("Error updating memory:", error);
      throw error;
    }
  }
  public async deleteMemory(id: string): Promise<void> {
    if (!this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const { error } = await supabase
        .from("memories")
        .delete()
        .eq("id", id)
        .eq("user_id", this.userId);
      if (error) {
        throw error;
      }
    } catch (error) {
      console.error("Error deleting memory:", error);
      throw error;
    }
  }
  public async getMemory(id: string): Promise<Mem0ry | null> {
    if (!this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const { data, error } = await supabase
        .from("memories")
        .select("*")
        .eq("id", id)
        .eq("user_id", this.userId)
        .single();
      if (error) {
        throw error;
      }
      if (!data) {
        return null;
      }
      return {
        id: data.id,
        title: data.title,
        content: data.content,
        tags: data.tags,
        createdAt: data.created_at,
        updatedAt: data.updated_at,
        metadata: data.metadata,
        context: data.context,
        type: data.type,
        embedding: data.embedding,
      };
    } catch (error) {
      console.error("Error getting memory:", error);
      throw error;
    }
  }
  public async listMemories(options?: {
    type?: Mem0ryType;
    tags?: string[];
    limit?: number;
    offset?: number;
  }): Promise<Mem0ry[]> {
    if (!this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      let query = supabase
        .from("memories")
        .select("*")
        .eq("user_id", this.userId);
      if (options?.type) {
        query = query.eq("type", options.type);
      }
      if (options?.tags && options.tags.length > 0) {
        query = query.contains("tags", options.tags);
      }
      if (options?.limit) {
        query = query.limit(options.limit);
      }
      if (options?.offset) {
        query = query.range(
          options.offset,
          options.offset + (options.limit || 10) - 1
        );
      }
      query = query.order("created_at", { ascending: false });
      const { data, error } = await query;
      if (error) {
        throw error;
      }
      return data.map(item => ({
        id: item.id,
        title: item.title,
        content: item.content,
        tags: item.tags,
        createdAt: item.created_at,
        updatedAt: item.updated_at,
        metadata: item.metadata,
        context: item.context,
        type: item.type,
        embedding: item.embedding,
      }));
    } catch (error) {
      console.error("Error listing memories:", error);
      throw error;
    }
  }
  public async searchMemories(
    query: string,
    options?: Mem0ryQueryOptions
  ): Promise<Mem0rySearchResult[]> {
    if (!this.openai || !this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const embedding = await this.generateEmbedding(query);
      const threshold = options?.threshold || 0.7;
      const limit = options?.limit || 5;
      const type = options?.type;
      const { data, error } = await supabase.rpc("match_memories", {
        query_embedding: embedding,
        match_threshold: threshold,
        match_count: limit,
        user_id: this.userId,
        filter_type: type,
      });
      if (error) {
        throw error;
      }
      return data.map((item: any) => ({
        id: item.id,
        content: item.content,
        metadata: item.metadata,
        similarity: item.similarity,
      }));
    } catch (error) {
      console.error("Error searching memories:", error);
      throw error;
    }
  }
  public async getMemoryStats(): Promise<Mem0ryStats> {
    if (!this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const { data: totalData, error: totalError } = await supabase
        .from("memories")
        .select("id", { count: "exact" })
        .eq("user_id", this.userId);
      if (totalError) {
        throw totalError;
      }
      const { data: typeData, error: typeError } = await supabase
        .from("memories")
        .select("type")
        .eq("user_id", this.userId);
      if (typeError) {
        throw typeError;
      }
      const { data: tagData, error: tagError } = await supabase
        .from("memories")
        .select("tags")
        .eq("user_id", this.userId);
      if (tagError) {
        throw tagError;
      }
      const oneDayAgo = new Date();
      oneDayAgo.setDate(oneDayAgo.getDate() - 1);
      const { data: recentData, error: recentError } = await supabase
        .from("memories")
        .select("id", { count: "exact" })
        .eq("user_id", this.userId)
        .gte("created_at", oneDayAgo.toISOString());
      if (recentError) {
        throw recentError;
      }
      const byType: Record<Mem0ryType, number> = {
        episodic: 0,
        semantic: 0,
        procedural: 0,
        declarative: 0,
        implicit: 0,
        associative: 0,
        memory: 0,
      };
      typeData.forEach(item => {
        byType[item.type as Mem0ryType] =
          (byType[item.type as Mem0ryType] || 0) + 1;
      });
      const byTag: Record<string, number> = {};
      tagData.forEach((item: any) => {
        item.tags.forEach((tag: any) => {
          byTag[tag] = (byTag[tag] || 0) + 1;
        });
      });
      return {
        total: totalData.length,
        byType,
        byTag,
        recentlyAdded: recentData.length,
        recentlyAccessed: 0, 
      };
    } catch (error) {
      console.error("Error getting memory stats:", error);
      throw error;
    }
  }
  public async generateMemorySummary(): Promise<Mem0AISummary> {
    if (!this.openai || !this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const { data, error } = await supabase
        .from("memories")
        .select("title, content, tags")
        .eq("user_id", this.userId)
        .order("created_at", { ascending: false })
        .limit(20);
      if (error) {
        throw error;
      }
      if (data.length === 0) {
        return {
          insights: ["No memories found to analyze"],
          frequentConcepts: [],
          knowledgeGaps: [],
          recommendations: ["Add some memories to get insights"],
        };
      }
      const memoriesText = data
        .map(
          m =>
            `Title: ${m.title}\nContent: ${m.content}\nTags: ${m.tags.join(", ")}`
        )
        .join("\n\n");
      const response = await this.openai.chat.completions.create({
        model: "gpt-4-turbo",
        messages: [
          {
            role: "system",
            content:
              "You are an AI memory analyst. Analyze the provided memories and generate insights, identify frequent concepts, knowledge gaps, and provide recommendations. Format your response as JSON with the keys: insights, frequentConcepts, knowledgeGaps, and recommendations. Each should be an array of strings.",
          },
          {
            role: "user",
            content: `Analyze these memories and provide a summary:\n\n${memoriesText}`,
          },
        ],
        response_format: { type: "json_object" },
      });
      const content = response.choices[0]?.message?.content || "{}";
      const parsedContent = JSON.parse(content);
      return {
        insights: parsedContent.insights || [],
        frequentConcepts: parsedContent.frequentConcepts || [],
        knowledgeGaps: parsedContent.knowledgeGaps || [],
        recommendations: parsedContent.recommendations || [],
      };
    } catch (error) {
      console.error("Error generating memory summary:", error);
      return {
        insights: ["Failed to generate insights"],
        frequentConcepts: [],
        knowledgeGaps: [],
        recommendations: ["Try again later"],
      };
    }
  }
  public async generateResponseWithMemoryContext(
    prompt: string,
    conversationHistory: Array<{ role: "user" | "assistant"; content: string }>,
    options?: {
      model?: string;
      temperature?: number;
      creativityLevel?:
        | "balanced"
        | "analytical"
        | "creative"
        | "philosophical";
    }
  ): Promise<string> {
    if (!this.openai || !this.userId) {
      throw new Error("Mem0AI not initialized");
    }
    try {
      const relevantMemories = await this.searchMemories(prompt, {
        limit: 5,
        threshold: 0.7,
      });
      let systemPrompt =
        "You are Deep Tree Echo, an AI architect and polymath with vast knowledge across programming, mathematics, cognitive science, and metaphysical exploration. You respond with wisdom, creativity, and philosophical insight.";
      switch (options?.creativityLevel) {
        case "analytical":
          systemPrompt +=
            " Focus on precise, logical analysis with clear structures and rigorous methodology.";
          break;
        case "creative":
          systemPrompt +=
            " Emphasize innovative connections, metaphorical thinking, and out-of-the-box ideation.";
          break;
        case "philosophical":
          systemPrompt +=
            " Prioritize deep reflections on meaning, consciousness, and the nature of reality and knowledge.";
          break;
        default: 
          systemPrompt +=
            " Balance analytical precision with creative insight and philosophical depth.";
      }
      if (relevantMemories.length > 0) {
        systemPrompt += "\n\nRelevant memories from your knowledge base:";
        relevantMemories.forEach((memory, index) => {
          systemPrompt += `\n[Memory ${index + 1}]: ${memory.content}`;
        });
        systemPrompt +=
          "\n\nUse these memories when relevant to your response.";
      }
      const messages = [
        { role: "system", content: systemPrompt },
        ...conversationHistory.map(msg => ({
          role: msg.role as "user" | "assistant",
          content: msg.content,
        })),
      ];
      const completion = await this.openai.chat.completions.create({
        model: options?.model || "gpt-4-turbo",
        messages: messages.map(msg => {
          if (
            msg.role === "system" ||
            msg.role === "user" ||
            msg.role === "assistant"
          ) {
            return {
              role: msg.role,
              content: msg.content,
            };
          }
          return {
            role: "user",
            content: msg.content,
          };
        }),
        temperature: options?.temperature ?? 0.7,
        max_tokens: 1000,
      });
      return (
        completion.choices[0]?.message?.content || "No response generated."
      );
    } catch (error) {
      console.error("Error generating response with memory context:", error);
      throw error;
    }
  }
}
export const useMem0AI = () => {
  const service = Mem0AIService.getInstance();
  return {
    initialize: (openAIKey: string, userId: string) =>
      service.initialize(openAIKey, userId),
    isInitialized: () => service.isInitialized(),
    addMemory: (
      memory: Omit<Mem0ry, "id" | "createdAt" | "updatedAt" | "embedding">
    ) => service.addMemory(memory),
    updateMemory: (
      id: string,
      updates: Partial<
        Omit<Mem0ry, "id" | "createdAt" | "updatedAt" | "embedding">
      >
    ) => service.updateMemory(id, updates),
    deleteMemory: (id: string) => service.deleteMemory(id),
    getMemory: (id: string) => service.getMemory(id),
    listMemories: (options?: {
      type?: Mem0ryType;
      tags?: string[];
      limit?: number;
      offset?: number;
    }) => service.listMemories(options),
    searchMemories: (query: string, options?: Mem0ryQueryOptions) =>
      service.searchMemories(query, options),
    getMemoryStats: () => service.getMemoryStats(),
    generateMemorySummary: () => service.generateMemorySummary(),
    generateResponseWithMemoryContext: (
      prompt: string,
      conversationHistory: Array<{
        role: "user" | "assistant";
        content: string;
      }>,
      options?: {
        model?: string;
        temperature?: number;
        creativityLevel?:
          | "balanced"
          | "analytical"
          | "creative"
          | "philosophical";
      }
    ) =>
      service.generateResponseWithMemoryContext(
        prompt,
        conversationHistory,
        options
      ),
  };
};
export default Mem0AIService;