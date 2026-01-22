import pytest
from unittest.mock import AsyncMock, patch
from cli.main import format_bar, format_awareness_level
def test_format_bar():
    assert format_bar(0.0, 10) == ' ' * 10
    assert format_bar(1.0, 10) == '█' * 10
    assert format_bar(0.5, 10) == '█' * 5 + ' ' * 5
def test_format_awareness_level():
    assert format_awareness_level(0.95) == 'Highly Self-Aware'
    assert format_awareness_level(0.75) == 'Moderately Self-Aware'
    assert format_awareness_level(0.55) == 'Developing Self-Awareness'
    assert format_awareness_level(0.35) == 'Limited Self-Awareness'