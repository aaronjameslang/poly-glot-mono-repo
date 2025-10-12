import pytest
import coloured_triangles


@pytest.mark.parametrize(
    "input_str,expected",
    [
        ("B", "B"),
        ("GB", "R"),
        ("RRR", "R"),
        ("RGBG", "B"),
        ("RBRGBRB", "G"),
        ("RBRGBRBGGRRRBGBBBGG", "G"),
        ("BGRGRBGBRRBBGRBGBBRBRGBRG", "B"),
        ("GRBGRRRBGRBGRGBRGBRBRGBRRGRBGRGBB", "R"),
        ("RBGRBGBRGBRBRGGRBBGRBGBRBBGRBGGBRBGBBGRBGBRGRBGRBB", "G"),
        (
            "BGBGRBGRRBGRBGGGRBGRGBGRRGGRBGRGRBGBRGBGBGRGBGBGBGRRBRGRRGBGRGBRGRBGRBGRBBGBRGRGRBGRGBRGBBRGGBRBGGRB",
            "G",
        ),
    ],
)
def test_coloured_triangles(input_str: str, expected: str):
    """Test the coloured triangles solver with various inputs."""
    assert coloured_triangles.solve(input_str) == expected
