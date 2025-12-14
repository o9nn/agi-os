#!/usr/bin/env python3
"""
Demo script for Enhanced Server-Side Streaming Responses

This script demonstrates the new streaming capabilities implemented for
Deep Tree Echo System Network (DTESN) processing, including timeout
prevention and large dataset optimization.

Run with: python demo_streaming_responses.py
"""

import asyncio
import json
import time
from typing import AsyncGenerator, Dict, Any

# Mock implementation for demonstration (doesn't require full Aphrodite setup)
class MockDTESNProcessor:
    """Mock processor for demonstrating streaming functionality."""
    
    async def process_streaming_chunks(
        self,
        input_data: str,
        membrane_depth: int = 3,
        esn_size: int = 256,
        chunk_size: int = 1024,
        enable_compression: bool = True,
        timeout_prevention: bool = True
    ) -> AsyncGenerator[Dict[str, Any], None]:
        """Demonstrate enhanced streaming with timeout prevention."""
        
        request_id = f"demo_stream_{int(time.time() * 1000000)}"
        total_length = len(input_data)
        processed_length = 0
        start_time = time.time()
        
        print(f"🚀 Starting enhanced streaming demo for {total_length} bytes")
        
        # Initial metadata
        yield {
            "type": "metadata",
            "request_id": request_id,
            "total_length": total_length,
            "chunk_size": chunk_size,
            "estimated_chunks": (total_length + chunk_size - 1) // chunk_size,
            "timeout_prevention_enabled": timeout_prevention,
            "compression_enabled": enable_compression,
            "large_dataset_mode": total_length > 10000
        }
        
        chunk_count = 0
        last_heartbeat = start_time
        
        for chunk_index in range(0, total_length, chunk_size):
            chunk_data = input_data[chunk_index:chunk_index + chunk_size]
            processed_length += len(chunk_data)
            chunk_count += 1
            current_time = time.time()
            
            # Timeout prevention heartbeat
            if timeout_prevention and (current_time - last_heartbeat) > 2:  # 2s for demo
                yield {
                    "type": "heartbeat",
                    "request_id": request_id,
                    "timestamp": current_time,
                    "progress": processed_length / total_length,
                    "chunks_processed": chunk_count - 1,
                    "server_rendered": True
                }
                last_heartbeat = current_time
                print(f"💓 Heartbeat - Progress: {(processed_length/total_length)*100:.1f}%")
            
            # Process chunk
            chunk_result = {
                "chunk_hash": hash(chunk_data) % 1000000,
                "chunk_length": len(chunk_data),
                "processed": True
            }
            
            # Create response
            chunk_response = {
                "type": "chunk",
                "request_id": request_id,
                "chunk_index": chunk_count - 1,
                "chunk_data": chunk_data[:50] + "..." if len(chunk_data) > 50 else chunk_data,
                "result": chunk_result,
                "progress": processed_length / total_length,
                "server_rendered": True,
                "compressed": enable_compression
            }
            
            yield chunk_response
            print(f"📦 Processed chunk {chunk_count}: {len(chunk_data)} bytes")
            
            # Small delay for demo
            await asyncio.sleep(0.1)
        
        # Completion
        end_time = time.time()
        total_duration = (end_time - start_time) * 1000
        
        yield {
            "type": "completion",
            "request_id": request_id,
            "total_chunks_processed": chunk_count,
            "total_processing_time_ms": total_duration,
            "bytes_processed": processed_length,
            "throughput_bytes_per_sec": processed_length / max((end_time - start_time), 0.001),
            "server_streaming_complete": True
        }
        
        print(f"✅ Streaming completed: {processed_length} bytes in {total_duration:.1f}ms")

    async def process_large_dataset_stream(
        self,
        input_data: str,
        max_chunk_size: int = 4096,
        compression_level: int = 1
    ) -> AsyncGenerator[Dict[str, Any], None]:
        """Demonstrate large dataset streaming with aggressive optimization."""
        
        request_id = f"large_demo_{int(time.time() * 1000000)}"
        total_length = len(input_data)
        
        print(f"🏎️  Starting large dataset streaming demo for {total_length} bytes")
        
        # Optimized metadata
        yield {
            "type": "large_dataset_metadata",
            "request_id": request_id,
            "total_size_bytes": total_length,
            "optimal_chunk_size": max_chunk_size,
            "compression_level": compression_level,
            "server_optimized": True
        }
        
        processed_length = 0
        chunk_index = 0
        start_time = time.time()
        
        for offset in range(0, total_length, max_chunk_size):
            chunk_data = input_data[offset:offset + max_chunk_size]
            processed_length += len(chunk_data)
            current_time = time.time()
            
            # Enhanced heartbeat for large datasets
            if (current_time - start_time) > 3 and chunk_index % 5 == 0:  # Every 5 chunks for demo
                yield {
                    "type": "large_dataset_heartbeat",
                    "request_id": request_id,
                    "progress": processed_length / total_length,
                    "bytes_processed": processed_length,
                    "elapsed_time_ms": (current_time - start_time) * 1000,
                    "est_completion_time_ms": ((current_time - start_time) / max(processed_length / total_length, 0.01)) * 1000
                }
                print(f"💓 Large dataset heartbeat - {(processed_length/total_length)*100:.1f}% complete")
            
            # Optimized chunk response
            chunk_response = {
                "type": "large_dataset_chunk",
                "i": chunk_index,  # Shortened field names
                "d": chunk_data[:20] + "..." if len(chunk_data) > 20 else chunk_data,
                "p": round(processed_length / total_length, 4),
                "t": round((current_time - start_time) * 1000, 1)
            }
            
            yield chunk_response
            print(f"⚡ Large dataset chunk {chunk_index}: {len(chunk_data)} bytes (optimized)")
            
            chunk_index += 1
            await asyncio.sleep(0.05)  # Minimal delay for maximum throughput
        
        # Optimized completion
        end_time = time.time()
        total_duration = (end_time - start_time) * 1000
        
        yield {
            "type": "large_dataset_completion",
            "request_id": request_id,
            "total_chunks": chunk_index,
            "total_bytes": processed_length,
            "duration_ms": total_duration,
            "throughput_mb_per_sec": (processed_length / 1024 / 1024) / max((end_time - start_time), 0.001),
            "server_optimized": True
        }
        
        print(f"🏁 Large dataset streaming completed: {total_duration:.1f}ms")


async def demo_standard_streaming():
    """Demo standard enhanced streaming with timeout prevention."""
    print("\n" + "="*60)
    print("🎬 DEMO 1: Enhanced Standard Streaming")
    print("="*60)
    
    processor = MockDTESNProcessor()
    test_data = "This is a test of enhanced server-side streaming with timeout prevention. " * 50
    
    print(f"📊 Input: {len(test_data)} bytes")
    print("🔧 Features: Timeout prevention, compression, adaptive chunking")
    print()
    
    async for chunk in processor.process_streaming_chunks(
        input_data=test_data,
        chunk_size=500,
        enable_compression=True,
        timeout_prevention=True
    ):
        # Simulate SSE format
        event_type = chunk.get("type", "data")
        print(f"event: {event_type}")
        print(f"data: {json.dumps(chunk, indent=2)[:200]}...")
        print()
        
        if chunk.get("type") == "completion":
            break


async def demo_large_dataset_streaming():
    """Demo large dataset streaming with aggressive optimization."""
    print("\n" + "="*60)
    print("🎬 DEMO 2: Large Dataset Streaming")
    print("="*60)
    
    processor = MockDTESNProcessor()
    large_data = "Large dataset streaming test with aggressive optimization features. " * 500
    
    print(f"📊 Input: {len(large_data)} bytes (large dataset mode)")
    print("🔧 Features: Aggressive optimization, enhanced compression, 20s heartbeats")
    print()
    
    async for chunk in processor.process_large_dataset_stream(
        input_data=large_data,
        max_chunk_size=2048,
        compression_level=2
    ):
        # Simulate SSE format with events
        if chunk.get("type") == "large_dataset_metadata":
            print("event: metadata")
        elif "heartbeat" in chunk.get("type", ""):
            print("event: heartbeat")
        elif "completion" in chunk.get("type", ""):
            print("event: completion")
        else:
            print("event: chunk")
            
        print(f"data: {json.dumps(chunk, indent=2)[:200]}...")
        print()
        
        if "completion" in chunk.get("type", ""):
            break


async def demo_timeout_prevention():
    """Demo timeout prevention mechanisms."""
    print("\n" + "="*60)
    print("🎬 DEMO 3: Timeout Prevention")
    print("="*60)
    
    print("⏱️  Simulating long-running operation that would normally timeout...")
    print("💓 Watch for heartbeat messages preventing timeout")
    print()
    
    processor = MockDTESNProcessor()
    timeout_test_data = "Timeout prevention test data. " * 200
    
    heartbeat_count = 0
    async for chunk in processor.process_streaming_chunks(
        input_data=timeout_test_data,
        chunk_size=100,
        timeout_prevention=True
    ):
        if chunk.get("type") == "heartbeat":
            heartbeat_count += 1
            print(f"💓 HEARTBEAT #{heartbeat_count} - Keeping connection alive")
            print(f"   Progress: {chunk.get('progress', 0)*100:.1f}%")
            print(f"   Timestamp: {chunk.get('timestamp', 0)}")
            print()
        elif chunk.get("type") == "completion":
            print(f"✅ Operation completed successfully with {heartbeat_count} heartbeats")
            break


def print_header():
    """Print demo header."""
    print("🌊" * 30)
    print("🚀 Enhanced Server-Side Streaming Demo")
    print("🌊" * 30)
    print()
    print("This demo shows the new streaming capabilities:")
    print("• ⏱️  Timeout Prevention with automatic heartbeats")
    print("• 📦 Enhanced chunked processing with compression")
    print("• 🏎️  Large dataset optimization (>1MB)")
    print("• 🔧 Adaptive performance tuning")
    print("• 🛡️  Enhanced error handling and recovery")
    print()


def print_footer():
    """Print demo footer."""
    print("\n" + "🌊" * 30)
    print("✨ Demo Complete!")
    print("🌊" * 30)
    print()
    print("🔗 Key Benefits:")
    print("• No more timeout errors on large operations")
    print("• Efficient streaming of datasets up to GBs")
    print("• Real-time progress tracking")
    print("• Optimized bandwidth usage with compression")
    print("• Graceful error handling and recovery")
    print()
    print("📖 See STREAMING_RESPONSE_GUIDE.md for full documentation")
    print("🧪 Run tests with: pytest tests/endpoints/test_performance_integration.py")


async def main():
    """Run all streaming demos."""
    print_header()
    
    try:
        await demo_standard_streaming()
        await demo_large_dataset_streaming()
        await demo_timeout_prevention()
    except KeyboardInterrupt:
        print("\n\n⚠️  Demo interrupted by user")
    except Exception as e:
        print(f"\n\n❌ Demo error: {e}")
    finally:
        print_footer()


if __name__ == "__main__":
    asyncio.run(main())