from pathlib import Path


class SaveLoadMixin:
    """Class for loading and saving pydantic models to files."""

    @classmethod
    def load(cls, filename: str | Path):
        return cls.model_validate_json(Path(filename).read_text())  # type: ignore

    def save(self, filename: str | Path, allow_overwrite: bool = False):
        print("Got here")
        if not allow_overwrite and Path(filename).exists():
            raise FileExistsError(f"File already exists: {filename}")
        print("Got here")
        Path(filename).write_text(self.model_dump_json(indent=2))  # type: ignore
