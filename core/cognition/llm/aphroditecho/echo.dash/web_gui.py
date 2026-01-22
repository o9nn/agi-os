import argparse
import io
import json
import logging
import random
import sys
import threading
import time
from datetime import datetime, timedelta
from pathlib import Path
import contextlib
try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    MATPLOTLIB_AVAILABLE = True
except ImportError:
    MATPLOTLIB_AVAILABLE = False
    plt = None
try:
    import networkx as nx
    NETWORKX_AVAILABLE = True
except ImportError:
    NETWORKX_AVAILABLE = False
    nx = None
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    np = None
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
    psutil = None
try:
    from flask import Flask, jsonify, request, Response
    FLASK_AVAILABLE = True
except ImportError:
    FLASK_AVAILABLE = False
    Flask = jsonify = request = Response = None
try:
    from adaptive_heartbeat import AdaptiveHeartbeat
    ADAPTIVE_HEARTBEAT_AVAILABLE = True
except ImportError:
    ADAPTIVE_HEARTBEAT_AVAILABLE = False
    AdaptiveHeartbeat = None
try:
    from memory_management import HypergraphMemory
    HYPERGRAPH_MEMORY_AVAILABLE = True
except ImportError:
    HYPERGRAPH_MEMORY_AVAILABLE = False
    HypergraphMemory = None
try:
    from deep_tree_echo import DeepTreeEcho
except ImportError:
    DeepTreeEcho = None
try:
    from activity_regulation import ActivityRegulator, TaskPriority
except ImportError:
    ActivityRegulator = None
    TaskPriority = None
try:
    from chat_session_manager import session_manager
except ImportError:
    session_manager = None
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', filename='web_gui.log', filemode='a')
logger = logging.getLogger(__name__)
if FLASK_AVAILABLE:
    app = Flask(__name__)
else:
    class DummyApp:
        def route(self, *args, **kwargs):
            def decorator(func):
                return func
            return decorator
        def run(self, *args, **kwargs):
            print('Flask not available - cannot start web server')
    app = DummyApp()
MEMORY = None
ACTIVITY_REGULATOR = None
HEARTBEAT_SYSTEM = AdaptiveHeartbeat() if ADAPTIVE_HEARTBEAT_AVAILABLE else None
HEARTBEAT_THREAD = None
def start_heartbeat_thread():
    global HEARTBEAT_THREAD
    if HEARTBEAT_SYSTEM and (HEARTBEAT_THREAD is None or not HEARTBEAT_THREAD.is_alive()):
        HEARTBEAT_THREAD = threading.Thread(target=HEARTBEAT_SYSTEM.start, daemon=True)
        HEARTBEAT_THREAD.start()
        logger.info('Started adaptive heartbeat thread')
    elif not ADAPTIVE_HEARTBEAT_AVAILABLE:
        logger.warning('Adaptive heartbeat not available - missing dependencies')
SYSTEM_HISTORY = {'cpu': [], 'memory': [], 'disk': [], 'network': [], 'heartbeat': [], 'timestamps': []}
HEARTBEAT_LOGS = []
HTML_TEMPLATE = '\n<!DOCTYPE html>\n<html>\n<head>\n    <title>Echo System Dashboard</title>\n    <meta charset="UTF-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1">\n    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css">\n    <style>\n        body {\n            font-family: \'Segoe UI\', Tahoma, Geneva, Verdana, sans-serif;\n            margin: 0;\n            padding: 0;\n            background-color: #f5f5f5;\n            color: #333;\n        }\n        .container {\n            width: 95%;\n            margin: 0 auto;\n            padding: 20px 0;\n        }\n        .header {\n            background-color: #2c3e50;\n            color: white;\n            padding: 10px 20px;\n            display: flex;\n            justify-content: space-between;\n            align-items: center;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.2);\n        }\n        .tabs {\n            display: flex;\n            margin-top: 20px;\n            border-bottom: 1px solid #ddd;\n        }\n        .tab {\n            padding: 10px 20px;\n            cursor: pointer;\n            border: 1px solid transparent;\n            border-bottom: none;\n            border-radius: 5px 5px 0 0;\n            margin-right: 5px;\n        }\n        .tab.active {\n            background-color: white;\n            border-color: #ddd;\n            color: #2c3e50;\n            font-weight: bold;\n        }\n        .tab:hover:not(.active) {\n            background-color: #eee;\n        }\n        .tab-content {\n            display: none;\n            background-color: white;\n            padding: 20px;\n            border: 1px solid #ddd;\n            border-top: none;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n        }\n        .tab-content.active {\n            display: block;\n        }\n        .status-container {\n            display: flex;\n            justify-content: space-between;\n            flex-wrap: wrap;\n            margin-bottom: 20px;\n        }\n        .status-card {\n            background-color: white;\n            border-radius: 5px;\n            padding: 15px;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n            flex: 1;\n            min-width: 200px;\n            margin: 0 10px 10px 0;\n        }\n        .status-card h3 {\n            margin-top: 0;\n            color: #2c3e50;\n        }\n        .chart-container {\n            background-color: white;\n            border-radius: 5px;\n            padding: 15px;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n            margin-bottom: 20px;\n        }\n        .chart-container h3 {\n            margin-top: 0;\n            color: #2c3e50;\n        }\n        .chart {\n            width: 100%;\n            height: 300px;\n            margin-top: 15px;\n        }\n        .chart img {\n            width: 100%;\n            height: 100%;\n            object-fit: contain;\n        }\n        table {\n            width: 100%;\n            border-collapse: collapse;\n        }\n        th, td {\n            padding: 10px;\n            text-align: left;\n            border-bottom: 1px solid #ddd;\n        }\n        th {\n            background-color: #f2f2f2;\n        }\n        tr:hover {\n            background-color: #f5f5f5;\n        }\n        .control-panel {\n            display: flex;\n            flex-wrap: wrap;\n            gap: 15px;\n            margin-top: 15px;\n        }\n        .control-card {\n            background-color: #f9f9f9;\n            border-radius: 5px;\n            padding: 15px;\n            box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n            flex: 1;\n            min-width: 250px;\n        }\n        .btn {\n            padding: 8px 15px;\n            background-color: #2c3e50;\n            color: white;\n            border: none;\n            border-radius: 4px;\n            cursor: pointer;\n            font-size: 14px;\n            margin-top: 10px;\n        }\n        .btn:hover {\n            background-color: #1a252f;\n        }\n        .btn.danger {\n            background-color: #e74c3c;\n        }\n        .btn.danger:hover {\n            background-color: #c0392b;\n        }\n        .btn.warning {\n            background-color: #f39c12;\n        }\n        .btn.warning:hover {\n            background-color: #d35400;\n        }\n        .btn.success {\n            background-color: #27ae60;\n        }\n        .btn.success:hover {\n            background-color: #2ecc71;\n        }\n        .slider-container {\n            margin-top: 10px;\n        }\n        input[type="range"] {\n            width: 100%;\n        }\n        .value-display {\n            text-align: center;\n            margin-top: 5px;\n            font-weight: bold;\n        }\n        .log-entry {\n            padding: 8px;\n            margin-bottom: 5px;\n            border-radius: 4px;\n        }\n        .log-entry:nth-child(odd) {\n            background-color: #f9f9f9;\n        }\n        .status-indicator {\n            display: inline-block;\n            width: 12px;\n            height: 12px;\n            border-radius: 50%;\n            margin-right: 10px;\n        }\n        .status-good {\n            background-color: #2ecc71;\n        }\n        .status-warning {\n            background-color: #f39c12;\n        }\n        .status-critical {\n            background-color: #e74c3c;\n        }\n        /* Heartbeat tab specific styles */\n        .pulse-animation {\n            animation: pulse 1s infinite;\n            display: inline-block;\n            color: #e74c3c;\n        }\n        @keyframes pulse {\n            0% { transform: scale(1); }\n            50% { transform: scale(1.2); }\n            100% { transform: scale(1); }\n        }\n        .heartbeat-controls {\n            display: grid;\n            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));\n            gap: 15px;\n        }\n        /* Chat Sessions tab specific styles */\n        .session-card {\n            background-color: #f8f9fa;\n            border: 1px solid #dee2e6;\n            border-radius: 8px;\n            padding: 15px;\n            margin-bottom: 15px;\n            cursor: pointer;\n            transition: all 0.3s ease;\n        }\n        .session-card:hover {\n            background-color: #e9ecef;\n            box-shadow: 0 4px 8px rgba(0,0,0,0.1);\n        }\n        .session-header {\n            display: flex;\n            justify-content: space-between;\n            align-items: center;\n            margin-bottom: 10px;\n        }\n        .session-title {\n            font-weight: bold;\n            color: #2c3e50;\n            font-size: 16px;\n        }\n        .session-platform {\n            background-color: #3498db;\n            color: white;\n            padding: 4px 8px;\n            border-radius: 12px;\n            font-size: 12px;\n            text-transform: uppercase;\n        }\n        .session-meta {\n            font-size: 14px;\n            color: #6c757d;\n            margin-bottom: 8px;\n        }\n        .session-preview {\n            font-size: 14px;\n            color: #495057;\n            max-height: 60px;\n            overflow: hidden;\n            text-overflow: ellipsis;\n        }\n        .session-stats {\n            display: flex;\n            gap: 15px;\n            margin-top: 10px;\n            font-size: 12px;\n            color: #6c757d;\n        }\n        .filter-controls {\n            display: flex;\n            gap: 15px;\n            margin-bottom: 20px;\n            flex-wrap: wrap;\n        }\n        .filter-control {\n            display: flex;\n            flex-direction: column;\n            gap: 5px;\n        }\n        .filter-control label {\n            font-weight: bold;\n            color: #2c3e50;\n        }\n        .filter-control select, .filter-control input {\n            padding: 8px;\n            border: 1px solid #ddd;\n            border-radius: 4px;\n        }\n        .session-detail {\n            background-color: white;\n            border: 1px solid #dee2e6;\n            border-radius: 8px;\n            padding: 20px;\n            margin-bottom: 20px;\n        }\n        .message-card {\n            background-color: #f8f9fa;\n            border-left: 4px solid #3498db;\n            padding: 15px;\n            margin-bottom: 15px;\n            border-radius: 0 8px 8px 0;\n        }\n        .message-card.user {\n            border-left-color: #27ae60;\n            background-color: #e8f5e8;\n        }\n        .message-card.assistant {\n            border-left-color: #3498db;\n            background-color: #e3f2fd;\n        }\n        .message-header {\n            display: flex;\n            justify-content: space-between;\n            margin-bottom: 10px;\n            font-size: 14px;\n        }\n        .message-role {\n            font-weight: bold;\n            text-transform: capitalize;\n        }\n        .message-content {\n            line-height: 1.6;\n            white-space: pre-wrap;\n        }\n        .pagination {\n            display: flex;\n            justify-content: center;\n            align-items: center;\n            gap: 10px;\n            margin-top: 20px;\n        }\n        .pagination button {\n            padding: 8px 12px;\n            background-color: #3498db;\n            color: white;\n            border: none;\n            border-radius: 4px;\n            cursor: pointer;\n        }\n        .pagination button:disabled {\n            background-color: #bdc3c7;\n            cursor: not-allowed;\n        }\n        .pagination span {\n            font-weight: bold;\n        }\n    </style>\n</head>\n<body>\n    <div class="header">\n        <h1><i class="fas fa-project-diagram"></i> Echo System Dashboard</h1>\n        <div>\n            <span id="current-time"></span>\n        </div>\n    </div>\n\n    <div class="container">\n        <div class="tabs">\n            <div class="tab active" data-tab="overview">Overview</div>\n            <div class="tab" data-tab="resources">System Resources</div>\n            <div class="tab" data-tab="heartbeat">Adaptive Heartbeat</div>\n            <div class="tab" data-tab="logs">Activity Logs</div>\n            <div class="tab" data-tab="network">Network</div>\n            <div class="tab" data-tab="chatsessions">Chat Sessions</div>\n            <div class="tab" data-tab="config">Configuration</div>\n        </div>\n\n        <!-- Overview Tab -->\n        <div id="overview" class="tab-content active">\n            <div class="status-container">\n                <div class="status-card">\n                    <h3>System Status</h3>\n                    <p><strong>Status:</strong> <span id="system-status">Operational</span></p>\n                    <p><strong>Uptime:</strong> <span id="system-uptime">Loading...</span></p>\n                </div>\n                <div class="status-card">\n                    <h3>Resource Usage</h3>\n                    <p><strong>CPU:</strong> <span id="cpu-usage">Loading...</span></p>\n                    <p><strong>Memory:</strong> <span id="memory-usage">Loading...</span></p>\n                    <p><strong>Disk:</strong> <span id="disk-usage">Loading...</span></p>\n                </div>\n                <div class="status-card">\n                    <h3>Heartbeat Status</h3>\n                    <p><strong>Rate:</strong> <span id="overview-heartbeat-rate">Loading...</span></p>\n                    <p><strong>Mode:</strong> <span id="overview-heartbeat-mode">Normal</span></p>\n                </div>\n                <div class="status-card">\n                    <h3>Recent Events</h3>\n                    <div id="recent-events">Loading events...</div>\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>System Overview</h3>\n                <div class="chart">\n                    <img src="/chart/system_overview" alt="System Overview" id="system-overview-chart">\n                </div>\n            </div>\n        </div>\n\n        <!-- Resources Tab -->\n        <div id="resources" class="tab-content">\n            <div class="status-container">\n                <div class="status-card">\n                    <h3>CPU</h3>\n                    <div id="cpu-detail">\n                        <p><strong>Usage:</strong> <span id="cpu-percent">Loading...</span></p>\n                        <p><strong>Cores:</strong> <span id="cpu-cores">Loading...</span></p>\n                    </div>\n                </div>\n                <div class="status-card">\n                    <h3>Memory</h3>\n                    <div id="memory-detail">\n                        <p><strong>Used:</strong> <span id="memory-used">Loading...</span></p>\n                        <p><strong>Available:</strong> <span id="memory-available">Loading...</span></p>\n                        <p><strong>Total:</strong> <span id="memory-total">Loading...</span></p>\n                    </div>\n                </div>\n                <div class="status-card">\n                    <h3>Disk</h3>\n                    <div id="disk-detail">\n                        <p><strong>Used:</strong> <span id="disk-used">Loading...</span></p>\n                        <p><strong>Free:</strong> <span id="disk-free">Loading...</span></p>\n                        <p><strong>Total:</strong> <span id="disk-total">Loading...</span></p>\n                    </div>\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>CPU History</h3>\n                <div class="chart">\n                    <img src="/chart/cpu_history" alt="CPU History" id="cpu-chart">\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Memory History</h3>\n                <div class="chart">\n                    <img src="/chart/memory_history" alt="Memory History" id="memory-chart">\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Process List</h3>\n                <table id="process-table">\n                    <thead>\n                        <tr>\n                            <th>PID</th>\n                            <th>Name</th>\n                            <th>CPU %</th>\n                            <th>Memory %</th>\n                            <th>Status</th>\n                        </tr>\n                    </thead>\n                    <tbody id="process-body">\n                        <tr>\n                            <td colspan="5">Loading processes...</td>\n                        </tr>\n                    </tbody>\n                </table>\n            </div>\n        </div>\n\n        <!-- Adaptive Heartbeat Tab -->\n        <div id="heartbeat" class="tab-content">\n            <div class="status-container">\n                <div class="status-card">\n                    <h3><i class="fas fa-heartbeat pulse-animation"></i> Current Heartbeat</h3>\n                    <p><strong>Rate:</strong> <span id="heartbeat-rate">Loading...</span> BPM</p>\n                    <p><strong>System Status:</strong> <span id="heartbeat-status">\n                        <span class="status-indicator status-good"></span>Normal\n                    </span></p>\n                    <p><strong>Hyper Drive:</strong> <span id="hyper-drive-status">Inactive</span></p>\n                </div>\n\n                <div class="status-card">\n                    <h3>System Health</h3>\n                    <p><strong>CPU Usage:</strong> <span id="heartbeat-cpu">Loading...</span></p>\n                    <p><strong>Last Assessment:</strong> <span id="last-assessment">Loading...</span></p>\n                    <button id="force-assessment" class="btn">Force Assessment</button>\n                </div>\n\n                <div class="status-card">\n                    <h3>Quick Actions</h3>\n                    <button id="toggle-hyper-drive" class="btn warning">Toggle Hyper Drive</button>\n                    <button id="restart-heartbeat" class="btn danger">Restart Heartbeat</button>\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Heartbeat History</h3>\n                <p>Showing heartbeat rate over time with hyper drive periods highlighted in yellow</p>\n                <div class="chart">\n                    <img src="/chart/heartbeat_history" alt="Heartbeat History" id="heartbeat-chart">\n                </div>\n            </div>\n\n            <div class="control-panel">\n                <div class="control-card">\n                    <h3>Base Heartbeat Rate</h3>\n                    <p>Adjust the base heartbeat rate (beats per minute)</p>\n                    <div class="slider-container">\n                        <input type="range" id="base-rate-slider" min="30" max="120" value="60">\n                        <div class="value-display"><span id="base-rate-value">60</span> BPM</div>\n                    </div>\n                    <button id="update-base-rate" class="btn">Update</button>\n                </div>\n\n                <div class="control-card">\n                    <h3>Hyper Drive Threshold</h3>\n                    <p>Set CPU threshold for automatic Hyper Drive activation</p>\n                    <div class="slider-container">\n                        <input type="range" id="hyper-threshold-slider" min="60" max="95" value="90">\n                        <div class="value-display"><span id="hyper-threshold-value">90</span>%</div>\n                    </div>\n                    <button id="update-hyper-threshold" class="btn">Update</button>\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Recent Heartbeat Events</h3>\n                <div id="heartbeat-events">\n                    <p>Loading events...</p>\n                </div>\n            </div>\n        </div>\n\n        <!-- Logs Tab -->\n        <div id="logs" class="tab-content">\n            <div class="chart-container">\n                <h3>System Log</h3>\n                <div id="log-content">\n                    <p>Loading logs...</p>\n                </div>\n            </div>\n        </div>\n\n        <!-- Network Tab -->\n        <div id="network" class="tab-content">\n            <div class="status-container">\n                <div class="status-card">\n                    <h3>Network Status</h3>\n                    <p><strong>Status:</strong> <span id="network-status">Connected</span></p>\n                </div>\n                <div class="status-card">\n                    <h3>Network Traffic</h3>\n                    <p><strong>Sent:</strong> <span id="network-sent">Loading...</span></p>\n                    <p><strong>Received:</strong> <span id="network-received">Loading...</span></p>\n                </div>\n                <div class="status-card">\n                    <h3>Active Connections</h3>\n                    <p><strong>Count:</strong> <span id="connection-count">Loading...</span></p>\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Network History</h3>\n                <div class="chart">\n                    <img src="/chart/network_history" alt="Network History" id="network-chart">\n                </div>\n            </div>\n\n            <div class="chart-container">\n                <h3>Connection List</h3>\n                <table id="connection-table">\n                    <thead>\n                        <tr>\n                            <th>Local Address</th>\n                            <th>Remote Address</th>\n                            <th>Status</th>\n                            <th>Type</th>\n                        </tr>\n                    </thead>\n                    <tbody id="connection-body">\n                        <tr>\n                            <td colspan="4">Loading connections...</td>\n                        </tr>\n                    </tbody>\n                </table>\n            </div>\n        </div>\n\n        <!-- Chat Sessions Tab -->\n        <div id="chatsessions" class="tab-content">\n            <div class="chart-container">\n                <h3>Chat Session Manager</h3>\n\n                <!-- Statistics Summary -->\n                <div class="status-container" id="chat-stats-container">\n                    <div class="status-card">\n                        <h4>Total Sessions</h4>\n                        <p id="total-sessions">Loading...</p>\n                    </div>\n                    <div class="status-card">\n                        <h4>Total Messages</h4>\n                        <p id="total-messages">Loading...</p>\n                    </div>\n                    <div class="status-card">\n                        <h4>Average Session Length</h4>\n                        <p id="avg-session-length">Loading...</p>\n                    </div>\n                    <div class="status-card">\n                        <h4>Active Platforms</h4>\n                        <p id="active-platforms">Loading...</p>\n                    </div>\n                </div>\n\n                <!-- Filters -->\n                <div class="filter-controls">\n                    <div class="filter-control">\n                        <label for="platform-filter">Platform:</label>\n                        <select id="platform-filter">\n                            <option value="">All Platforms</option>\n                            <option value="CHATGPT">ChatGPT</option>\n                            <option value="CLAUDE">Claude</option>\n                            <option value="WINDSURF">Windsurf</option>\n                            <option value="BROWSER">Browser</option>\n                            <option value="API">API</option>\n                        </select>\n                    </div>\n                    <div class="filter-control">\n                        <label for="days-filter">Time Range:</label>\n                        <select id="days-filter">\n                            <option value="7">Last 7 days</option>\n                            <option value="30" selected>Last 30 days</option>\n                            <option value="90">Last 90 days</option>\n                            <option value="365">Last year</option>\n                        </select>\n                    </div>\n                    <div class="filter-control">\n                        <label for="search-filter">Search:</label>\n                        <input type="text" id="search-filter" placeholder="Search sessions...">\n                    </div>\n                    <div class="filter-control">\n                        <label>&nbsp;</label>\n                        <button id="apply-filters" class="btn">Apply Filters</button>\n                    </div>\n                </div>\n\n                <!-- Session List -->\n                <div id="sessions-container">\n                    <div id="sessions-list">\n                        <p>Loading chat sessions...</p>\n                    </div>\n\n                    <!-- Pagination -->\n                    <div class="pagination" id="pagination-container" style="display: none;">\n                        <button id="prev-page" onclick="loadSessions(currentPage - 1)">Previous</button>\n                        <span id="page-info">Page 1 of 1</span>\n                        <button id="next-page" onclick="loadSessions(currentPage + 1)">Next</button>\n                    </div>\n                </div>\n\n                <!-- Session Detail Modal (initially hidden) -->\n                <div id="session-detail" class="session-detail" style="display: none;">\n                    <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;">\n                        <h4 id="detail-title">Session Details</h4>\n                        <button onclick="closeSessionDetail()" class="btn">Close</button>\n                    </div>\n                    <div id="session-info"></div>\n                    <div id="session-messages"></div>\n                </div>\n            </div>\n        </div>\n\n        <!-- Configuration Tab -->\n        <div id="config" class="tab-content">\n            <div class="chart-container">\n                <h3>System Configuration</h3>\n                <table id="config-table">\n                    <thead>\n                        <tr>\n                            <th>Setting</th>\n                            <th>Value</th>\n                            <th>Action</th>\n                        </tr>\n                    </thead>\n                    <tbody id="config-body">\n                        <tr>\n                            <td colspan="3">Loading configuration...</td>\n                        </tr>\n                    </tbody>\n                </table>\n            </div>\n        </div>\n    </div>\n\n    <script>\n        // Tab switching functionality\n        document.querySelectorAll(\'.tab\').forEach(tab => {\n            tab.addEventListener(\'click\', function() {\n                // Remove active class from all tabs\n                document.querySelectorAll(\'.tab\').forEach(t => t.classList.remove(\'active\'));\n                document.querySelectorAll(\'.tab-content\').forEach(c => c.classList.remove(\'active\'));\n\n                // Add active class to clicked tab\n                this.classList.add(\'active\');\n                document.getElementById(this.dataset.tab).classList.add(\'active\');\n\n                // Load specific data based on tab\n                const activeTab = this.dataset.tab;\n                if (activeTab === \'chatsessions\') {\n                    loadChatStatistics();\n                    loadSessions(1, {});\n                }\n\n                // Refresh charts on tab change\n                updateCharts();\n            });\n        });\n\n        // Current time update\n        function updateCurrentTime() {\n            const now = new Date();\n            document.getElementById(\'current-time\').textContent = now.toLocaleString();\n        }\n\n        // Update charts and refresh data\n        function updateCharts() {\n            const activeTab = document.querySelector(\'.tab.active\').dataset.tab;\n\n            // Add timestamp to force refresh of images\n            const timestamp = new Date().getTime();\n\n            if (activeTab === \'overview\' || activeTab === \'resources\') {\n                document.getElementById(\'cpu-chart\').src = \'/chart/cpu_history?\' + timestamp;\n                document.getElementById(\'memory-chart\').src = \'/chart/memory_history?\' + timestamp;\n                document.getElementById(\'system-overview-chart\').src = \'/chart/system_overview?\' + timestamp;\n            }\n\n            if (activeTab === \'network\') {\n                document.getElementById(\'network-chart\').src = \'/chart/network_history?\' + timestamp;\n            }\n\n            if (activeTab === \'heartbeat\') {\n                document.getElementById(\'heartbeat-chart\').src = \'/chart/heartbeat_history?\' + timestamp;\n            }\n        }\n\n        // Fetch and update system metrics\n        function updateSystemMetrics() {\n            fetch(\'/api/system_metrics\')\n                .then(response => response.json())\n                .then(data => {\n                    document.getElementById(\'cpu-usage\').textContent = data.cpu + \'%\';\n                    document.getElementById(\'cpu-percent\').textContent = data.cpu + \'%\';\n                    document.getElementById(\'memory-usage\').textContent = data.memory + \'%\';\n                    document.getElementById(\'disk-usage\').textContent = data.disk + \'%\';\n                    document.getElementById(\'system-uptime\').textContent = data.uptime;\n                    document.getElementById(\'cpu-cores\').textContent = data.cpu_cores;\n                    document.getElementById(\'memory-used\').textContent = data.memory_used;\n                    document.getElementById(\'memory-available\').textContent = data.memory_available;\n                    document.getElementById(\'memory-total\').textContent = data.memory_total;\n                    document.getElementById(\'disk-used\').textContent = data.disk_used;\n                    document.getElementById(\'disk-free\').textContent = data.disk_free;\n                    document.getElementById(\'disk-total\').textContent = data.disk_total;\n\n                    // Update process table\n                    const processBody = document.getElementById(\'process-body\');\n                    processBody.innerHTML = \'\';\n                    data.processes.forEach(process => {\n                        const row = document.createElement(\'tr\');\n                        row.innerHTML = `\n                            <td>${process.pid}</td>\n                            <td>${process.name}</td>\n                            <td>${process.cpu}%</td>\n                            <td>${process.memory}%</td>\n                            <td>${process.status}</td>\n                        `;\n                        processBody.appendChild(row);\n                    });\n                })\n                .catch(error => console.error(\'Error fetching system metrics:\', error));\n        }\n\n        // Fetch and update network metrics\n        function updateNetworkMetrics() {\n            fetch(\'/api/network_metrics\')\n                .then(response => response.json())\n                .then(data => {\n                    document.getElementById(\'network-sent\').textContent = data.sent;\n                    document.getElementById(\'network-received\').textContent = data.received;\n                    document.getElementById(\'connection-count\').textContent = data.connections;\n\n                    // Update connection table\n                    const connectionBody = document.getElementById(\'connection-body\');\n                    connectionBody.innerHTML = \'\';\n                    data.connection_list.forEach(conn => {\n                        const row = document.createElement(\'tr\');\n                        row.innerHTML = `\n                            <td>${conn.local_address}</td>\n                            <td>${conn.remote_address}</td>\n                            <td>${conn.status}</td>\n                            <td>${conn.type}</td>\n                        `;\n                        connectionBody.appendChild(row);\n                    });\n                })\n                .catch(error => console.error(\'Error fetching network metrics:\', error));\n        }\n\n        // Fetch and update system logs\n        function updateSystemLogs() {\n            fetch(\'/api/system_logs\')\n                .then(response => response.json())\n                .then(data => {\n                    const logContent = document.getElementById(\'log-content\');\n                    logContent.innerHTML = \'\';\n                    data.logs.forEach(log => {\n                        const logEntry = document.createElement(\'div\');\n                        logEntry.className = \'log-entry\';\n                        logEntry.innerHTML = `<strong>${log.timestamp}</strong>: ${log.message}`;\n                        logContent.appendChild(logEntry);\n                    });\n                })\n                .catch(error => console.error(\'Error fetching system logs:\', error));\n        }\n\n        // Fetch and update recent events\n        function updateRecentEvents() {\n            fetch(\'/api/recent_events\')\n                .then(response => response.json())\n                .then(data => {\n                    const eventsDiv = document.getElementById(\'recent-events\');\n                    eventsDiv.innerHTML = \'\';\n                    data.events.forEach(event => {\n                        const eventEntry = document.createElement(\'div\');\n                        eventEntry.className = \'log-entry\';\n                        eventEntry.innerHTML = `<strong>${event.timestamp}</strong>: ${event.message}`;\n                        eventsDiv.appendChild(eventEntry);\n                    });\n                })\n                .catch(error => console.error(\'Error fetching recent events:\', error));\n        }\n\n        // Fetch and update heartbeat metrics\n        function updateHeartbeatMetrics() {\n            fetch(\'/api/heartbeat_metrics\')\n                .then(response => response.json())\n                .then(data => {\n                    // Update heartbeat tab\n                    document.getElementById(\'heartbeat-rate\').textContent = data.current_rate;\n                    document.getElementById(\'heartbeat-cpu\').textContent = data.cpu_usage + \'%\';\n                    document.getElementById(\'hyper-drive-status\').textContent = data.hyper_drive ? \'Active\' : \'Inactive\';\n\n                    // Update overview tab heartbeat status\n                    document.getElementById(\'overview-heartbeat-rate\').textContent = data.current_rate + \' BPM\';\n                    document.getElementById(\'overview-heartbeat-mode\').textContent = data.hyper_drive ? \'Hyper Drive\' : \'Normal\';\n\n                    // Update heartbeat status indicator\n                    const statusIndicator = document.getElementById(\'heartbeat-status\');\n                    if (data.health_status === \'Good\') {\n                        statusIndicator.innerHTML = \'<span class="status-indicator status-good"></span>Normal\';\n                    } else if (data.health_status === \'Warning\') {\n                        statusIndicator.innerHTML = \'<span class="status-indicator status-warning"></span>Elevated\';\n                    } else if (data.health_status === \'Critical\') {\n                        statusIndicator.innerHTML = \'<span class="status-indicator status-critical"></span>Critical\';\n                    }\n\n                    // Update heartbeat events\n                    const eventsDiv = document.getElementById(\'heartbeat-events\');\n                    eventsDiv.innerHTML = \'\';\n                    data.recent_logs.forEach(log => {\n                        const logEntry = document.createElement(\'div\');\n                        logEntry.className = \'log-entry\';\n                        logEntry.innerHTML = `<strong>${log.timestamp}</strong>: ${log.message}`;\n                        eventsDiv.appendChild(logEntry);\n                    });\n                })\n                .catch(error => console.error(\'Error fetching heartbeat metrics:\', error));\n        }\n\n        // Setup heartbeat controls\n        function setupHeartbeatControls() {\n            // Base rate slider\n            const baseRateSlider = document.getElementById(\'base-rate-slider\');\n            const baseRateValue = document.getElementById(\'base-rate-value\');\n\n            baseRateSlider.addEventListener(\'input\', () => {\n                baseRateValue.textContent = baseRateSlider.value;\n            });\n\n            document.getElementById(\'update-base-rate\').addEventListener(\'click\', () => {\n                const value = baseRateSlider.value;\n                fetch(`/api/update_heartbeat_rate?value=${value}`)\n                    .then(response => response.json())\n                    .then(data => {\n                        if (data.success) {\n                            alert(\'Base heartbeat rate updated!\');\n                        } else {\n                            alert(\'Error: \' + data.message);\n                        }\n                    })\n                    .catch(error => console.error(\'Error updating heartbeat rate:\', error));\n            });\n\n            // Hyper drive threshold slider\n            const hyperThresholdSlider = document.getElementById(\'hyper-threshold-slider\');\n            const hyperThresholdValue = document.getElementById(\'hyper-threshold-value\');\n\n            hyperThresholdSlider.addEventListener(\'input\', () => {\n                hyperThresholdValue.textContent = hyperThresholdSlider.value;\n            });\n\n            document.getElementById(\'update-hyper-threshold\').addEventListener(\'click\', () => {\n                const value = hyperThresholdSlider.value;\n                fetch(`/api/update_hyper_threshold?value=${value}`)\n                    .then(response => response.json())\n                    .then(data => {\n                        if (data.success) {\n                            alert(\'Hyper drive threshold updated!\');\n                        } else {\n                            alert(\'Error: \' + data.message);\n                        }\n                    })\n                    .catch(error => console.error(\'Error updating threshold:\', error));\n            });\n\n            // Toggle hyper drive button\n            document.getElementById(\'toggle-hyper-drive\').addEventListener(\'click\', () => {\n                fetch(\'/api/toggle_hyper_drive\')\n                    .then(response => response.json())\n                    .then(data => {\n                        if (data.success) {\n                            alert(data.message);\n                            updateHeartbeatMetrics();\n                        } else {\n                            alert(\'Error: \' + data.message);\n                        }\n                    })\n                    .catch(error => console.error(\'Error toggling hyper drive:\', error));\n            });\n\n            // Force assessment button\n            document.getElementById(\'force-assessment\').addEventListener(\'click\', () => {\n                fetch(\'/api/force_heartbeat_assessment\')\n                    .then(response => response.json())\n                    .then(data => {\n                        if (data.success) {\n                            alert(\'System assessment completed!\');\n                            updateHeartbeatMetrics();\n                        } else {\n                            alert(\'Error: \' + data.message);\n                        }\n                    })\n                    .catch(error => console.error(\'Error forcing assessment:\', error));\n            });\n\n            // Restart heartbeat button\n            document.getElementById(\'restart-heartbeat\').addEventListener(\'click\', () => {\n                if (confirm(\'Are you sure you want to restart the heartbeat system?\')) {\n                    fetch(\'/api/restart_heartbeat\')\n                        .then(response => response.json())\n                        .then(data => {\n                            if (data.success) {\n                                alert(\'Heartbeat system restarted!\');\n                                updateHeartbeatMetrics();\n                            } else {\n                                alert(\'Error: \' + data.message);\n                            }\n                        })\n                        .catch(error => console.error(\'Error restarting heartbeat:\', error));\n                }\n            });\n        }\n\n        // Chat Session Management\n        let currentPage = 1;\n        let currentFilters = {};\n\n        // Load chat sessions with pagination and filtering\n        function loadSessions(page = 1, filters = {}) {\n            currentPage = page;\n            currentFilters = filters;\n\n            const queryParams = new URLSearchParams({\n                page: page,\n                limit: 10,\n                platform: filters.platform || \'\',\n                days: filters.days || 30,\n                search: filters.search || \'\'\n            });\n\n            fetch(`/api/chat_sessions?${queryParams}`)\n                .then(response => response.json())\n                .then(data => {\n                    if (data.success) {\n                        displaySessions(data.sessions);\n                        updatePagination(data.page, data.total, data.limit, data.has_next);\n                    } else {\n                        document.getElementById(\'sessions-list\').innerHTML = `<p>Error loading sessions: ${data.error}</p>`;\n                    }\n                })\n                .catch(error => {\n                    console.error(\'Error loading sessions:\', error);\n                    document.getElementById(\'sessions-list\').innerHTML = \'<p>Error loading sessions. Chat session manager may not be available.</p>\';\n                });\n        }\n\n        // Display sessions in the UI\n        function displaySessions(sessions) {\n            const container = document.getElementById(\'sessions-list\');\n\n            if (!sessions || sessions.length === 0) {\n                container.innerHTML = \'<p>No chat sessions found.</p>\';\n                return;\n            }\n\n            container.innerHTML = sessions.map(session => `\n                <div class="session-card" onclick="loadSessionDetail(\'${session.id}\')">\n                    <div class="session-header">\n                        <div class="session-title">${session.title || \'Untitled Session\'}</div>\n                        <div class="session-platform">${session.platform}</div>\n                    </div>\n                    <div class="session-meta">\n                        Started: ${new Date(session.start_time).toLocaleString()} |\n                        ${session.end_time ? \'Ended: \' + new Date(session.end_time).toLocaleString() : \'Active\'} |\n                        Status: ${session.status}\n                    </div>\n                    <div class="session-preview">\n                        ${session.last_message_preview || \'No messages yet\'}\n                    </div>\n                    <div class="session-stats">\n                        <span>💬 ${session.total_messages} messages</span>\n                        <span>🔄 Echo: ${session.avg_echo_value ? session.avg_echo_value.toFixed(2) : \'N/A\'}</span>\n                        <span>⚡ Salience: ${session.avg_salience ? session.avg_salience.toFixed(2) : \'N/A\'}</span>\n                    </div>\n                </div>\n            `).join(\'\');\n        }\n\n        // Update pagination controls\n        function updatePagination(page, total, limit, hasNext) {\n            const container = document.getElementById(\'pagination-container\');\n            const pageInfo = document.getElementById(\'page-info\');\n            const prevBtn = document.getElementById(\'prev-page\');\n            const nextBtn = document.getElementById(\'next-page\');\n\n            const totalPages = Math.ceil(total / limit);\n\n            if (totalPages <= 1) {\n                container.style.display = \'none\';\n                return;\n            }\n\n            container.style.display = \'flex\';\n            pageInfo.textContent = `Page ${page} of ${totalPages} (${total} sessions)`;\n\n            prevBtn.disabled = page <= 1;\n            nextBtn.disabled = !hasNext;\n        }\n\n        // Load detailed session view\n        function loadSessionDetail(sessionId) {\n            fetch(`/api/chat_session/${sessionId}`)\n                .then(response => response.json())\n                .then(data => {\n                    if (data.success) {\n                        displaySessionDetail(data.session);\n                    } else {\n                        alert(\'Error loading session details: \' + data.error);\n                    }\n                })\n                .catch(error => {\n                    console.error(\'Error loading session detail:\', error);\n                    alert(\'Error loading session details\');\n                });\n        }\n\n        // Display detailed session view\n        function displaySessionDetail(session) {\n            const container = document.getElementById(\'session-detail\');\n            const title = document.getElementById(\'detail-title\');\n            const info = document.getElementById(\'session-info\');\n            const messages = document.getElementById(\'session-messages\');\n\n            title.textContent = session.title || \'Untitled Session\';\n\n            info.innerHTML = `\n                <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-bottom: 20px;">\n                    <div><strong>Platform:</strong> ${session.platform}</div>\n                    <div><strong>Status:</strong> ${session.status}</div>\n                    <div><strong>Started:</strong> ${new Date(session.start_time).toLocaleString()}</div>\n                    <div><strong>Ended:</strong> ${session.end_time ? new Date(session.end_time).toLocaleString() : \'Still active\'}</div>\n                    <div><strong>Messages:</strong> ${session.total_messages}</div>\n                    <div><strong>Conversation ID:</strong> ${session.conversation_id || \'N/A\'}</div>\n                </div>\n            `;\n\n            if (session.messages && session.messages.length > 0) {\n                messages.innerHTML = `\n                    <h4>Messages (${session.messages.length})</h4>\n                    ${session.messages.map(msg => `\n                        <div class="message-card ${msg.role}">\n                            <div class="message-header">\n                                <div class="message-role">${msg.role}</div>\n                                <div>${new Date(msg.timestamp).toLocaleString()}</div>\n                            </div>\n                            <div class="message-content">${msg.content}</div>\n                            <div style="margin-top: 10px; font-size: 12px; color: #6c757d;">\n                                Echo: ${msg.echo_value ? msg.echo_value.toFixed(3) : \'N/A\'} |\n                                Salience: ${msg.salience ? msg.salience.toFixed(3) : \'N/A\'}\n                            </div>\n                        </div>\n                    `).join(\'\')}\n                `;\n            } else {\n                messages.innerHTML = \'<h4>No messages in this session</h4>\';\n            }\n\n            container.style.display = \'block\';\n            container.scrollIntoView({ behavior: \'smooth\' });\n        }\n\n        // Close session detail view\n        function closeSessionDetail() {\n            document.getElementById(\'session-detail\').style.display = \'none\';\n        }\n\n        // Load chat statistics\n        function loadChatStatistics() {\n            fetch(\'/api/chat_statistics\')\n                .then(response => response.json())\n                .then(data => {\n                    if (data.success) {\n                        const stats = data.statistics;\n                        document.getElementById(\'total-sessions\').textContent = stats.total_sessions || 0;\n                        document.getElementById(\'total-messages\').textContent = stats.total_messages || 0;\n                        document.getElementById(\'avg-session-length\').textContent =\n                            stats.avg_session_length ? `${stats.avg_session_length.toFixed(1)} messages` : \'N/A\';\n                        document.getElementById(\'active-platforms\').textContent =\n                            stats.platforms_used ? stats.platforms_used.join(\', \') : \'None\';\n                    } else {\n                        console.log(\'Chat statistics not available:\', data.error);\n                    }\n                })\n                .catch(error => {\n                    console.error(\'Error loading chat statistics:\', error);\n                });\n        }\n\n        // Setup chat session event handlers\n        function setupChatSessionHandlers() {\n            // Apply filters button\n            document.getElementById(\'apply-filters\').addEventListener(\'click\', () => {\n                const filters = {\n                    platform: document.getElementById(\'platform-filter\').value,\n                    days: document.getElementById(\'days-filter\').value,\n                    search: document.getElementById(\'search-filter\').value\n                };\n                loadSessions(1, filters);\n            });\n\n            // Search on Enter key\n            document.getElementById(\'search-filter\').addEventListener(\'keypress\', (e) => {\n                if (e.key === \'Enter\') {\n                    document.getElementById(\'apply-filters\').click();\n                }\n            });\n        }\n\n        // Initialize everything\n        function initialize() {\n            updateCurrentTime();\n            updateSystemMetrics();\n            updateNetworkMetrics();\n            updateSystemLogs();\n            updateRecentEvents();\n            updateHeartbeatMetrics();\n            updateCharts();\n            setupHeartbeatControls();\n            setupChatSessionHandlers();\n\n            // Update time every second\n            setInterval(updateCurrentTime, 1000);\n\n            // Update metrics periodically\n            setInterval(updateSystemMetrics, 5000);\n            setInterval(updateNetworkMetrics, 5000);\n            setInterval(updateSystemLogs, 10000);\n            setInterval(updateRecentEvents, 5000);\n            setInterval(updateHeartbeatMetrics, 3000);\n            setInterval(updateCharts, 30000);\n        }\n\n        // Run initialization when page loads\n        window.addEventListener(\'load\', initialize);\n    </script>\n</body>\n</html>\n'
def parse_arguments():
    parser = argparse.ArgumentParser(description='Web-based GUI Dashboard for Deep Tree Echo')
    parser.add_argument('--port', type=int, default=8080, help='Port to run the web server on (default: 8080)')
    parser.add_argument('--debug', action='store_true', help='Run Flask in debug mode')
    parser.add_argument('--no-activity', action='store_true', help='Disable activity monitoring system')
    return parser.parse_args()
def get_system_metrics():
    try:
        if PSUTIL_AVAILABLE:
            cpu = psutil.cpu_percent(interval=None)
            memory_usage = psutil.virtual_memory().percent
            disk = psutil.disk_usage('/').percent
        else:
            cpu = 25.0
            memory_usage = 35.0
            disk = 45.0
        echo_metrics = {}
        try:
            if DeepTreeEcho and hasattr(DeepTreeEcho, 'get_instance'):
                echo = DeepTreeEcho.get_instance()
                echo_metrics = echo.analyze_echo_patterns()
        except (ImportError, AttributeError) as e:
            logger.warning('Error getting echo metrics: %s', str(e))
        return {'cpu': cpu, 'memory': memory_usage, 'disk': disk, 'echo_metrics': echo_metrics}
    except (OSError, IOError, ValueError) as e:
        logger.error('Error getting system metrics: %s', str(e))
        return {'cpu': 0, 'memory': 0, 'disk': 0}
def get_memory_stats():
    try:
        if MEMORY is None:
            return {'node_count': 0, 'edge_count': 0, 'avg_salience': 0.0}
        return MEMORY.generate_statistics()
    except (AttributeError, TypeError) as e:
        logger.error('Error getting memory stats: %s', str(e))
        return {'error': str(e)}
def get_recent_logs(max_logs=50):
    logs = []
    try:
        logs_dir = Path('activity_logs')
        if not logs_dir.exists():
            return {'logs': []}
        for component in logs_dir.iterdir():
            if component.is_dir():
                activity_file = component / 'activity.json'
                if activity_file.exists():
                    try:
                        with open(activity_file, encoding='utf-8') as f:
                            component_logs = json.load(f)
                            logs.extend(component_logs)
                    except (FileNotFoundError, json.JSONDecodeError, OSError) as e:
                        logger.error('Error reading logs from %s: %s', activity_file, str(e))
        logs.sort(key=lambda x: x.get('time', 0), reverse=True)
        logs = logs[:max_logs]
        return {'logs': logs}
    except (OSError, IOError, json.JSONDecodeError) as e:
        logger.error('Error getting logs: %s', str(e))
        return {'logs': []}
def _generate_system_history_data():
    if PSUTIL_AVAILABLE and NUMPY_AVAILABLE:
        return {'timestamps': [time.time() - i * 60 for i in range(30, 0, -1)], 'cpu': [psutil.cpu_percent() * 0.8 + 10 * np.sin(i / 3) for i in range(30)], 'memory': [psutil.virtual_memory().percent * 0.7 + 15 * np.sin(i / 5 + 2) for i in range(30)]}
    else:
        import math
        return {'timestamps': [time.time() - i * 60 for i in range(30, 0, -1)], 'cpu': [25.0 + 10 * math.sin(i / 3) for i in range(30)], 'memory': [35.0 + 15 * math.sin(i / 5 + 2) for i in range(30)]}
def _configure_chart_appearance(fig, ax):
    fig.patch.set_facecolor('#272741')
    ax.set_facecolor('#323250')
    ax.grid(True, linestyle='--', alpha=0.3)
    ax.tick_params(axis='x', colors='white', rotation=45)
    ax.tick_params(axis='y', colors='white')
    ax.set_ylim(0, 100)
    ax.legend(loc='upper left', facecolor='#272741', framealpha=0.8, labelcolor='white')
def generate_system_health_chart():
    if not MATPLOTLIB_AVAILABLE:
        return generate_error_image('Matplotlib not available')
    try:
        history = _generate_system_history_data()
        fig, ax = plt.subplots(figsize=(10, 6))
        _configure_chart_appearance(fig, ax)
        labels = [time.strftime('%H:%M', time.localtime(t)) for t in history['timestamps']]
        ax.plot(labels, history['cpu'], 'o-', color='#4f9cff', label='CPU Usage (%)')
        ax.plot(labels, history['memory'], 's-', color='#ff6e4a', label='Memory Usage (%)')
        ax.set_xlabel('Time', color='white')
        ax.set_ylabel('Usage (%)', color='white')
        ax.set_title('System Resource Usage Over Time', color='white', fontsize=14)
        plt.tight_layout()
        img_data = io.BytesIO()
        plt.savefig(img_data, format='png', dpi=100)
        img_data.seek(0)
        plt.close(fig)
        return img_data
    except (ValueError, KeyError, AttributeError, ImportError) as e:
        logger.error('Error generating system health chart: %s', str(e))
        return generate_error_image('Error generating system health chart')
def generate_echo_history_chart():
    if not MATPLOTLIB_AVAILABLE:
        return generate_error_image('Matplotlib not available')
    try:
        history = {'timestamps': [time.time() - i * 60 for i in range(30, 0, -1)], 'avg_echo': [0.4 + 0.2 * np.sin(i / 5) for i in range(30)], 'max_echo': [0.7 + 0.15 * np.sin(i / 4 + 1) for i in range(30)], 'resonant_nodes': [10 + 5 * np.sin(i / 6 + 2) for i in range(30)]}
        fig, ax = plt.subplots(figsize=(10, 6))
        fig.patch.set_facecolor('#272741')
        ax.set_facecolor('#323250')
        labels = [time.strftime('%H:%M', time.localtime(t)) for t in history['timestamps']]
        ax.plot(labels, history['avg_echo'], 'o-', color='#4f9cff', label='Avg Echo')
        ax.plot(labels, history['max_echo'], 's-', color='#9c4fff', label='Max Echo')
        ax2 = ax.twinx()
        ax2.plot(labels, history['resonant_nodes'], '^-', color='#ff6e4a', label='Resonant Nodes')
        ax.set_xlabel('Time', color='white')
        ax.set_ylabel('Echo Value', color='#4f9cff')
        ax2.set_ylabel('Node Count', color='#ff6e4a')
        ax.set_title('Echo Patterns Over Time', color='white', fontsize=14)
        ax.grid(True, linestyle='--', alpha=0.3)
        ax.tick_params(axis='x', colors='white', rotation=45)
        ax.tick_params(axis='y', colors='#4f9cff')
        ax2.tick_params(axis='y', colors='#ff6e4a')
        ax.set_ylim(0, 1)
        lines1, labels1 = ax.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax.legend(lines1 + lines2, labels1 + labels2, loc='upper left', facecolor='#272741', framealpha=0.8, labelcolor='white')
        plt.tight_layout()
        img_data = io.BytesIO()
        plt.savefig(img_data, format='png', dpi=100)
        img_data.seek(0)
        plt.close(fig)
        return img_data
    except (ValueError, KeyError, AttributeError, ImportError) as e:
        logger.error('Error generating echo history chart: %s', str(e))
        return generate_error_image('Error generating echo history chart')
def generate_memory_graph():
    if not MATPLOTLIB_AVAILABLE or not NETWORKX_AVAILABLE:
        return generate_error_image('Matplotlib or NetworkX not available')
    try:
        graph_instance = nx.DiGraph()
        if MEMORY is not None:
            nodes = list(MEMORY.nodes.items())[:100]
            for node_id, node in nodes:
                graph_instance.add_node(node_id, content=node.content[:50], salience=getattr(node, 'salience', 0.5), memory_type=str(getattr(node, 'memory_type', 'unknown')), size=300 + 700 * getattr(node, 'salience', 0.5))
            for edge in MEMORY.edges:
                if edge.from_id in graph_instance and edge.to_id in graph_instance:
                    graph_instance.add_edge(edge.from_id, edge.to_id, weight=edge.weight)
        else:
            for i in range(30):
                node_id = f'node_{i}'
                memory_types = ['episodic', 'semantic', 'procedural', 'working']
                graph_instance.add_node(node_id, content=f'Memory node {i}', salience=np.random.uniform(0.3, 0.9), memory_type=np.random.choice(memory_types), size=300 + 700 * np.random.uniform(0.3, 0.9))
            for i in range(50):
                from_id = f'node_{np.random.randint(0, 30)}'
                to_id = f'node_{np.random.randint(0, 30)}'
                if from_id != to_id:
                    graph_instance.add_edge(from_id, to_id, weight=np.random.uniform(0.3, 0.9))
        fig, ax = plt.subplots(figsize=(12, 8))
        fig.patch.set_facecolor('#272741')
        ax.set_facecolor('#272741')
        pos = nx.spring_layout(graph_instance)
        node_sizes = [data.get('size', 300) for _, data in graph_instance.nodes(data=True)]
        memory_types = [data.get('memory_type', 'unknown') for _, data in graph_instance.nodes(data=True)]
        unique_types = list(set(memory_types))
        color_map = plt.cm.get_cmap('tab10', len(unique_types))
        type_to_color = {t: color_map(i) for i, t in enumerate(unique_types)}
        node_colors = [type_to_color[t] for t in memory_types]
        nodes = nx.draw_networkx_nodes(graph_instance, pos, node_size=node_sizes, node_color=node_colors, alpha=0.8, ax=ax)
        edge_weights = [graph_instance[u][v].get('weight', 0.5) for u, v in graph_instance.edges()]
        nx.draw_networkx_edges(graph_instance, pos, edge_color=edge_weights, edge_cmap=plt.cm.Blues, width=1.5, alpha=0.6, arrows=True, ax=ax, arrowsize=10)
        legend_elements = [plt.Line2D([0], [0], marker='o', color='w', label=t, markerfacecolor=type_to_color[t], markersize=8) for t in unique_types]
        ax.legend(handles=legend_elements, title='Memory Types', loc='upper right', framealpha=0.8, facecolor='#272741', labelcolor='white')
        ax.set_title('Memory Hypergraph Visualization', color='white', fontsize=16)
        ax.set_axis_off()
        plt.tight_layout()
        img_data = io.BytesIO()
        plt.savefig(img_data, format='png', dpi=100)
        img_data.seek(0)
        plt.close(fig)
        return img_data
    except (ValueError, KeyError, AttributeError, ImportError, OSError) as e:
        logger.error('Error generating memory graph: %s', str(e))
        return generate_error_image('Error generating memory graph')
def generate_echo_network():
    try:
        echo_graph = nx.DiGraph()
        try:
            if DeepTreeEcho:
                echo = DeepTreeEcho() if not hasattr(DeepTreeEcho, 'get_instance') else DeepTreeEcho.get_instance()
                root_node = echo.root
            if root_node:
                def add_node_to_graph(node, parent_id=None):
                    node_id = id(node)
                    echo_graph.add_node(node_id, echo_value=getattr(node, 'echo_value', 0.5), content=getattr(node, 'content', 'Unknown'), size=300 + 1000 * getattr(node, 'echo_value', 0.5))
                    if parent_id is not None:
                        echo_graph.add_edge(parent_id, node_id)
                    for child in getattr(node, 'children', []):
                        add_node_to_graph(child, node_id)
                add_node_to_graph(root_node)
        except (ImportError, AttributeError, KeyError) as e:
            logger.warning('Could not get real echo tree, generating demo: %s', str(e))
            if len(echo_graph.nodes()) == 0:
                for i in range(20):
                    echo_graph.add_node(i, echo_value=np.random.uniform(0.2, 0.9), content=f'Node {i}', size=300 + 1000 * np.random.uniform(0.2, 0.9))
                for i in range(1, 20):
                    parent = (i - 1) // 3
                    echo_graph.add_edge(parent, i)
        fig, ax = plt.subplots(figsize=(10, 8))
        fig.patch.set_facecolor('#272741')
        ax.set_facecolor('#272741')
        if not echo_graph.nodes():
            ax.text(0.5, 0.5, 'No echo network data available', ha='center', va='center', color='white', fontsize=14)
            ax.set_axis_off()
        else:
            pos = nx.kamada_kawai_layout(echo_graph)
            node_sizes = [data.get('size', 300) for _, data in echo_graph.nodes(data=True)]
            echo_values = [data.get('echo_value', 0.5) for _, data in echo_graph.nodes(data=True)]
            cmap = plt.cm.viridis
            norm = plt.Normalize(vmin=0, vmax=1)
            nx.draw_networkx_nodes(echo_graph, pos, node_size=node_sizes, node_color=echo_values, cmap=cmap, alpha=0.9, ax=ax)
            nx.draw_networkx_edges(echo_graph, pos, edge_color='gray', alpha=0.5, arrows=True, arrowsize=10, ax=ax)
            sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
            sm.set_array([])
            cbar = fig.colorbar(sm, ax=ax, label='Echo Value')
            cbar.ax.yaxis.label.set_color('white')
            cbar.ax.tick_params(colors='white')
            ax.set_title('Deep Tree Echo Visualization', color='white', fontsize=16)
            ax.set_axis_off()
        plt.tight_layout()
        img_data = io.BytesIO()
        plt.savefig(img_data, format='png', dpi=100)
        img_data.seek(0)
        plt.close(fig)
        return img_data
    except (ValueError, KeyError, AttributeError, ImportError, OSError) as e:
        logger.error('Error generating echo network: %s', str(e))
        return generate_error_image('Error generating echo network visualization')
def generate_error_image(error_text):
    fig, ax = plt.subplots(figsize=(10, 6))
    fig.patch.set_facecolor('#272741')
    ax.set_facecolor('#272741')
    ax.text(0.5, 0.5, error_text, ha='center', va='center', color='white', fontsize=14)
    ax.set_axis_off()
    plt.tight_layout()
    img_data = io.BytesIO()
    plt.savefig(img_data, format='png', dpi=100)
    img_data.seek(0)
    plt.close(fig)
    return img_data
@app.route('/api/heartbeat_metrics')
def heartbeat_metrics():
    if not HEARTBEAT_SYSTEM:
        return jsonify({'current_rate': 60, 'hyper_drive': False, 'cpu_usage': 0, 'health_status': 'Unknown', 'recent_logs': [], 'error': 'Heartbeat system not available'})
    current_rate = HEARTBEAT_SYSTEM.current_heartbeat_rate
    hyper_drive = HEARTBEAT_SYSTEM.hyper_drive_active
    cpu_usage = HEARTBEAT_SYSTEM.last_cpu_usage
    if hyper_drive:
        health_status = 'Warning'
    elif cpu_usage > 80:
        health_status = 'Warning'
    elif cpu_usage > 95:
        health_status = 'Critical'
    else:
        health_status = 'Good'
    recent_logs = HEARTBEAT_LOGS[-10:] if HEARTBEAT_LOGS else []
    return jsonify({'current_rate': current_rate, 'hyper_drive': hyper_drive, 'cpu_usage': cpu_usage, 'health_status': health_status, 'recent_logs': recent_logs})
@app.route('/api/update_heartbeat_rate')
def update_heartbeat_rate():
    if not HEARTBEAT_SYSTEM:
        return jsonify({'success': False, 'message': 'Heartbeat system not available'})
    try:
        value = int(request.args.get('value', 60))
        HEARTBEAT_SYSTEM.base_heartbeat_rate = value
        log_heartbeat_event('Base heartbeat rate updated to %d BPM' % value)
        return jsonify({'success': True, 'message': 'Heartbeat rate updated to %d' % value})
    except (ValueError, TypeError) as e:
        logger.error('Error updating heartbeat rate: %s', str(e))
        return jsonify({'success': False, 'message': str(e)})
@app.route('/api/update_hyper_threshold')
def update_hyper_threshold():
    if not HEARTBEAT_SYSTEM:
        return jsonify({'success': False, 'message': 'Heartbeat system not available'})
    try:
        value = int(request.args.get('value', 90))
        HEARTBEAT_SYSTEM.hyper_drive_threshold = value
        log_heartbeat_event('Hyper drive threshold updated to %d%%' % value)
        return jsonify({'success': True, 'message': 'Threshold updated to %d%%' % value})
    except (ValueError, TypeError) as e:
        logger.error('Error updating hyper drive threshold: %s', str(e))
        return jsonify({'success': False, 'message': str(e)})
@app.route('/api/toggle_hyper_drive')
def toggle_hyper_drive():
    if not HEARTBEAT_SYSTEM:
        return jsonify({'success': False, 'message': 'Heartbeat system not available'})
    try:
        new_state = not HEARTBEAT_SYSTEM.hyper_drive_active
        HEARTBEAT_SYSTEM.hyper_drive_active = new_state
        log_heartbeat_event('Hyper drive manually %s' % ('activated' if new_state else 'deactivated'))
        return jsonify({'success': True, 'message': 'Hyper drive %s' % ('activated' if new_state else 'deactivated')})
    except (AttributeError, ValueError) as e:
        logger.error('Error toggling hyper drive: %s', str(e))
        return jsonify({'success': False, 'message': str(e)})
@app.route('/api/force_heartbeat_assessment')
def force_heartbeat_assessment():
    try:
        HEARTBEAT_SYSTEM.assess_system_state(force=True)
        log_heartbeat_event('Manual system assessment triggered')
        return jsonify({'success': True, 'message': 'Assessment completed'})
    except (AttributeError, ValueError) as e:
        logger.error('Error during system assessment: %s', str(e))
        return jsonify({'success': False, 'message': 'An internal error occurred.'})
@app.route('/api/restart_heartbeat')
def restart_heartbeat():
    try:
        if HEARTBEAT_THREAD and HEARTBEAT_THREAD.is_alive():
            HEARTBEAT_SYSTEM.stop_heartbeat()
            HEARTBEAT_THREAD.join(timeout=2.0)
        HEARTBEAT_SYSTEM.reset()
        start_heartbeat_thread()
        log_heartbeat_event('Heartbeat system restarted')
        return jsonify({'success': True, 'message': 'Heartbeat system restarted'})
    except (AttributeError, ImportError, ValueError) as e:
        logger.error('Error restarting heartbeat system: %s', str(e))
        return jsonify({'success': False, 'message': str(e)})
@app.route('/chart/heartbeat_history')
def heartbeat_history_chart():
    try:
        plt.figure(figsize=(10, 4))
        plt.plot(SYSTEM_HISTORY['timestamps'][-50:], SYSTEM_HISTORY['heartbeat'][-50:], 'r-', linewidth=2)
        hyper_periods = HEARTBEAT_SYSTEM.get_hyper_drive_periods()
        if hyper_periods:
            for period in hyper_periods[-5:]:
                start, end = period
                if end == 0:
                    end = datetime.now()
                plt.axvspan(start, end, color='yellow', alpha=0.3)
        plt.title('Heartbeat Rate History')
        plt.xlabel('Time')
        plt.ylabel('Rate (BPM)')
        plt.grid(True)
        plt.tight_layout()
        img = io.BytesIO()
        plt.savefig(img, format='png')
        img.seek(0)
        plt.close()
        return Response(img.getvalue(), content_type='image/png')
    except (ImportError, AttributeError, ValueError) as e:
        logger.error('Error generating heartbeat chart: %s', str(e))
        return Response(generate_error_image('Error generating chart').getvalue(), content_type='image/png')
def log_heartbeat_event(message):
    log_entry = {'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'), 'message': message}
    HEARTBEAT_LOGS.append(log_entry)
    if len(HEARTBEAT_LOGS) > 100:
        HEARTBEAT_LOGS.pop(0)
    logger.info('Heartbeat event: %s', message)
def update_metrics():
    while True:
        try:
            if not PSUTIL_AVAILABLE:
                cpu_percent = 25.0
                memory_percent = 35.0
                disk_percent = 45.0
                net_usage = 10.0
            else:
                cpu_percent = psutil.cpu_percent(interval=1)
                memory_percent = psutil.virtual_memory().percent
                disk_percent = psutil.disk_usage('/').percent
                net_io = psutil.net_io_counters()
                net_usage = (net_io.bytes_sent + net_io.bytes_recv) / 1024 / 1024
            current_time = datetime.now()
            SYSTEM_HISTORY['cpu'].append(cpu_percent)
            SYSTEM_HISTORY['memory'].append(memory_percent)
            SYSTEM_HISTORY['disk'].append(disk_percent)
            SYSTEM_HISTORY['network'].append(net_usage)
            SYSTEM_HISTORY['timestamps'].append(current_time)
            if HEARTBEAT_SYSTEM:
                SYSTEM_HISTORY['heartbeat'].append(HEARTBEAT_SYSTEM.current_heartbeat_rate)
            else:
                SYSTEM_HISTORY['heartbeat'].append(60.0)
            if len(SYSTEM_HISTORY['cpu']) > 1000:
                for key in SYSTEM_HISTORY:
                    SYSTEM_HISTORY[key] = SYSTEM_HISTORY[key][-1000:]
            time.sleep(2)
        except (OSError, IOError, AttributeError) as e:
            logger.error('Error in update_metrics: %s', str(e))
            time.sleep(5)
@app.route('/')
def index():
    return HTML_TEMPLATE
@app.route('/api/system_metrics')
def api_system_metrics():
    try:
        metrics = get_system_metrics()
        if PSUTIL_AVAILABLE:
            metrics.update({'cpu_cores': psutil.cpu_count(), 'memory_used': _format_bytes(psutil.virtual_memory().used), 'memory_available': _format_bytes(psutil.virtual_memory().available), 'memory_total': _format_bytes(psutil.virtual_memory().total), 'disk_used': _format_bytes(psutil.disk_usage('/').used), 'disk_free': _format_bytes(psutil.disk_usage('/').free), 'disk_total': _format_bytes(psutil.disk_usage('/').total), 'uptime': _format_uptime(time.time() - psutil.boot_time()), 'processes': _get_top_processes()})
        else:
            metrics.update({'cpu_cores': 'Unknown', 'memory_used': 'Unknown', 'memory_available': 'Unknown', 'memory_total': 'Unknown', 'disk_used': 'Unknown', 'disk_free': 'Unknown', 'disk_total': 'Unknown', 'uptime': 'Unknown', 'processes': []})
        return jsonify(metrics)
    except (OSError, IOError) as e:
        logger.error('Error getting system metrics: %s', str(e))
        return jsonify({'error': str(e)})
def _format_bytes(bytes_value):
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if bytes_value < 1024.0:
            return f'{bytes_value:.1f} {unit}'
        bytes_value /= 1024.0
    return f'{bytes_value:.1f} PB'
def _format_uptime(seconds):
    days = int(seconds // 86400)
    hours = int(seconds % 86400 // 3600)
    minutes = int(seconds % 3600 // 60)
    return f'{days}d {hours}h {minutes}m'
def _get_top_processes():
    if not PSUTIL_AVAILABLE:
        return []
    processes = []
    try:
        for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent', 'status']):
            try:
                processes.append({'pid': proc.info['pid'], 'name': proc.info['name'][:20], 'cpu': round(proc.info['cpu_percent'], 1), 'memory': round(proc.info['memory_percent'], 1), 'status': proc.info['status']})
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        return sorted(processes, key=lambda p: p['cpu'], reverse=True)[:10]
    except (OSError, IOError):
        return []
@app.route('/api/memory_stats')
def api_memory_stats():
    return jsonify({'stats': get_memory_stats()})
@app.route('/api/recent_logs')
def api_recent_logs():
    return jsonify(get_recent_logs())
@app.route('/api/system_logs')
def api_system_logs():
    try:
        logs = []
        log_data = get_recent_logs()
        for log_entry in log_data.get('logs', [])[:50]:
            logs.append({'timestamp': log_entry.get('time', 'Unknown'), 'message': log_entry.get('activity', 'No message')})
        return jsonify({'logs': logs})
    except (KeyError, TypeError) as e:
        logger.error('Error getting system logs: %s', str(e))
        return jsonify({'logs': []})
@app.route('/api/recent_events')
def api_recent_events():
    try:
        events = []
        if HEARTBEAT_LOGS:
            for log in HEARTBEAT_LOGS[-5:]:
                events.append({'timestamp': log.get('timestamp', 'Unknown'), 'message': log.get('message', 'No message')})
        return jsonify({'events': events})
    except (KeyError, TypeError) as e:
        logger.error('Error getting recent events: %s', str(e))
        return jsonify({'events': []})
@app.route('/api/network_metrics')
def api_network_metrics():
    try:
        if not PSUTIL_AVAILABLE:
            return jsonify({'sent': 'Unknown', 'received': 'Unknown', 'connections': 0, 'connection_list': [], 'error': 'psutil not available'})
        net_io = psutil.net_io_counters()
        connections = psutil.net_connections()
        sent_mb = net_io.bytes_sent / (1024 * 1024)
        received_mb = net_io.bytes_recv / (1024 * 1024)
        connection_list = []
        for conn in connections[:20]:
            with contextlib.suppress(AttributeError, ValueError):
                connection_list.append({'local_address': f'{conn.laddr.ip}:{conn.laddr.port}' if conn.laddr else 'Unknown', 'remote_address': f'{conn.raddr.ip}:{conn.raddr.port}' if conn.raddr else 'Unknown', 'status': conn.status, 'type': conn.type.name if hasattr(conn.type, 'name') else str(conn.type)})
        return jsonify({'sent': f'{sent_mb:.1f} MB', 'received': f'{received_mb:.1f} MB', 'connections': len(connections), 'connection_list': connection_list})
    except (OSError, IOError, AttributeError) as e:
        logger.error('Error getting network metrics: %s', str(e))
        return jsonify({'sent': 'Error', 'received': 'Error', 'connections': 0, 'connection_list': [], 'error': str(e)})
@app.route('/api/process_info')
def api_process_info():
    processes = []
    for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
        with contextlib.suppress(psutil.NoSuchProcess, psutil.AccessDenied):
            processes.append({'pid': proc.info['pid'], 'name': proc.info['name'], 'cpu_percent': proc.info['cpu_percent'], 'memory_percent': proc.info['memory_percent']})
    processes = sorted(processes, key=lambda p: p['cpu_percent'], reverse=True)[:10]
    return jsonify({'processes': processes})
@app.route('/api/tasks')
def api_tasks():
    try:
        tasks = []
        if ACTIVITY_REGULATOR:
            queue_copy = []
            for task in list(ACTIVITY_REGULATOR.task_queue.queue):
                queue_copy.append({'task_id': task.task_id, 'priority': task.priority.name if hasattr(task.priority, 'name') else str(task.priority), 'scheduled_time': task.scheduled_time})
            tasks = queue_copy
        return jsonify({'tasks': tasks})
    except Exception as e:
        logger.error('Error getting tasks: %s', str(e))
        return jsonify({'tasks': [], 'error': str(e)})
@app.route('/api/add_task')
def api_add_task():
    try:
        task_id = request.args.get('task_id', '')
        if not task_id or not ACTIVITY_REGULATOR:
            return jsonify({'success': False, 'error': 'Invalid task ID or activity regulator not available'})
        if TaskPriority:
            ACTIVITY_REGULATOR.add_task(task_id=task_id, callback=lambda: print(f'Executing {task_id}'), priority=TaskPriority.MEDIUM)
        return jsonify({'success': True})
    except Exception as e:
        logger.error('Error adding task: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/update_echo_threshold')
def api_update_echo_threshold():
    try:
        value = float(request.args.get('value', 0.75))
        try:
            if DeepTreeEcho:
                echo = DeepTreeEcho() if not hasattr(DeepTreeEcho, 'get_instance') else DeepTreeEcho.get_instance()
                echo.echo_threshold = value
        except Exception as e:
            logger.warning('Could not update echo threshold: %s', str(e))
        return jsonify({'success': True, 'value': value})
    except Exception as e:
        logger.error('Error updating threshold: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/inject_random_echo')
def api_inject_random_echo():
    try:
        echo = None
        try:
            if DeepTreeEcho:
                echo = DeepTreeEcho() if not hasattr(DeepTreeEcho, 'get_instance') else DeepTreeEcho.get_instance()
        except (ImportError, AttributeError) as e:
            logger.warning('Error initializing DeepTreeEcho: %s', str(e))
        if echo:
            all_nodes = []
            def collect_nodes(node):
                if node:
                    all_nodes.append(node)
                    for child in getattr(node, 'children', []):
                        collect_nodes(child)
            collect_nodes(echo.root)
            if len(all_nodes) > 1:
                source = random.choice(all_nodes)
                target = random.choice(all_nodes)
                while source == target:
                    target = random.choice(all_nodes)
                strength = random.uniform(0.3, 0.9)
                echo.inject_echo(source, target, strength)
                return jsonify({'success': True})
        return jsonify({'success': False, 'error': 'Echo system not available'})
    except Exception as e:
        logger.error('Error in inject_random_echo: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/propagate_echoes')
def api_propagate_echoes():
    try:
        try:
            if DeepTreeEcho:
                echo = DeepTreeEcho() if not hasattr(DeepTreeEcho, 'get_instance') else DeepTreeEcho.get_instance()
                echo.propagate_echoes()
                return jsonify({'success': True})
        except Exception as e:
            logger.warning('Could not propagate echoes: %s', str(e))
        return jsonify({'success': False, 'error': 'Echo system not available'})
    except Exception as e:
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/prune_weak_echoes')
def api_prune_weak_echoes():
    try:
        try:
            if DeepTreeEcho:
                echo = DeepTreeEcho() if not hasattr(DeepTreeEcho, 'get_instance') else DeepTreeEcho.get_instance()
                echo.prune_weak_echoes()
                return jsonify({'success': True})
        except Exception as e:
            logger.warning('Could not prune weak echoes: %s', str(e))
        return jsonify({'success': False, 'error': 'Echo system not available'})
    except Exception as e:
        logger.error('Error in prune_weak_echoes: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/chat_sessions')
def api_chat_sessions():
    try:
        sessions = []
        try:
            if session_manager:
                page = int(request.args.get('page', 1))
                limit = int(request.args.get('limit', 20))
                platform = request.args.get('platform')
                days = int(request.args.get('days', 30))
                sessions = session_manager.search_sessions(platform=platform, limit=limit * page, start_date=datetime.now() - timedelta(days=days))
            start = (page - 1) * limit
            end = start + limit
            paginated_sessions = sessions[start:end]
            return jsonify({'success': True, 'sessions': paginated_sessions, 'total': len(sessions), 'page': page, 'limit': limit, 'has_next': end < len(sessions)})
        except ImportError:
            return jsonify({'success': False, 'error': 'Chat session manager not available'})
    except Exception as e:
        logger.error('Error getting chat sessions: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/chat_session/<session_id>')
def api_chat_session_detail(session_id):
    try:
        try:
            if session_manager:
                session = session_manager.get_session(session_id)
                if not session:
                    return jsonify({'success': False, 'error': 'Session not found'})
            session_data = {'id': session.id, 'platform': session.platform.value, 'title': session.title, 'start_time': session.start_time, 'end_time': session.end_time, 'status': session.status.value, 'total_messages': session.total_messages, 'conversation_id': session.conversation_id, 'metadata': session.metadata, 'messages': []}
            for msg in session.messages:
                session_data['messages'].append({'id': msg.id, 'timestamp': msg.timestamp, 'role': msg.role, 'content': msg.content, 'echo_value': msg.echo_value, 'salience': msg.salience, 'metadata': msg.metadata})
            return jsonify({'success': True, 'session': session_data})
        except ImportError:
            return jsonify({'success': False, 'error': 'Chat session manager not available'})
    except Exception as e:
        logger.error('Error getting chat session detail: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/api/chat_statistics')
def api_chat_statistics():
    try:
        if session_manager:
            stats = session_manager.get_statistics()
            return jsonify({'success': True, 'statistics': stats})
        return jsonify({'success': False, 'error': 'Chat session manager not available'})
    except Exception as e:
        logger.error('Error getting chat statistics: %s', str(e))
        return jsonify({'success': False, 'error': str(e)})
@app.route('/chart/system_health')
def chart_system_health():
    img_data = generate_system_health_chart()
    return Response(img_data.getvalue(), mimetype='image/png')
@app.route('/chart/echo_history')
def chart_echo_history():
    img_data = generate_echo_history_chart()
    return Response(img_data.getvalue(), mimetype='image/png')
@app.route('/chart/memory_graph')
def chart_memory_graph():
    img_data = generate_memory_graph()
    return Response(img_data.getvalue(), mimetype='image/png')
@app.route('/chart/echo_network')
def chart_echo_network():
    img_data = generate_echo_network()
    return Response(img_data.getvalue(), mimetype='image/png')
@app.route('/chart/cpu_history')
def chart_cpu_history():
    return _generate_history_chart('cpu', 'CPU Usage (%)', '#4f9cff')
@app.route('/chart/memory_history')
def chart_memory_history():
    return _generate_history_chart('memory', 'Memory Usage (%)', '#ff6e4a')
@app.route('/chart/network_history')
def chart_network_history():
    return _generate_history_chart('network', 'Network Usage (MB)', '#9c4fff')
@app.route('/chart/system_overview')
def chart_system_overview():
    img_data = generate_system_health_chart()
    return Response(img_data.getvalue(), mimetype='image/png')
def _generate_history_chart(metric_key, ylabel, color):
    try:
        if not MATPLOTLIB_AVAILABLE:
            return Response(generate_error_image('Matplotlib not available').getvalue(), mimetype='image/png')
        fig, ax = plt.subplots(figsize=(10, 4))
        _configure_chart_appearance(fig, ax)
        recent_data = SYSTEM_HISTORY[metric_key][-50:] if SYSTEM_HISTORY[metric_key] else [0]
        recent_times = SYSTEM_HISTORY['timestamps'][-50:] if SYSTEM_HISTORY['timestamps'] else [datetime.now()]
        time_labels = [t.strftime('%H:%M') for t in recent_times]
        ax.plot(time_labels, recent_data, '-', color=color, linewidth=2)
        ax.set_title(f'{ylabel} History')
        ax.set_xlabel('Time')
        ax.set_ylabel(ylabel)
        plt.tight_layout()
        img = io.BytesIO()
        plt.savefig(img, format='png', dpi=100)
        img.seek(0)
        plt.close(fig)
        return Response(img.getvalue(), mimetype='image/png')
    except (ValueError, KeyError, AttributeError) as e:
        logger.error('Error generating %s chart: %s', metric_key, str(e))
        return Response(generate_error_image(f'Error generating {metric_key} chart').getvalue(), mimetype='image/png')
def main():
    global MEMORY, ACTIVITY_REGULATOR
    if not FLASK_AVAILABLE:
        logger.error('Flask is not available. Cannot start web server.')
        print('Error: Flask is required but not available. Please install Flask.')
        return 1
    args = parse_arguments()
    try:
        logger.info('Initializing memory system')
        MEMORY = HypergraphMemory(storage_dir='echo_memory')
        if not args.no_activity:
            logger.info('Initializing activity regulator')
            if ActivityRegulator:
                ACTIVITY_REGULATOR = ActivityRegulator()
        start_heartbeat_thread()
        metrics_thread = threading.Thread(target=update_metrics, daemon=True)
        metrics_thread.start()
        print('\n' + '=' * 80)
        print('Deep Tree Echo Dashboard Web Server')
        print(f'Running on http://localhost:{args.port}')
        print('Also try your forwarded port URLs:')
        print(f'- http://127.0.0.1:{args.port}')
        print(f'- http://localhost:{args.port}')
        print('=' * 80 + '\n')
        app.run(host='0.0.0.0', port=args.port, debug=args.debug)
    except Exception as e:
        logger.error('Error in main: %s', str(e), exc_info=True)
        print(f'Error: {e}')
        return 1
    return 0
if __name__ == '__main__':
    sys.exit(main())