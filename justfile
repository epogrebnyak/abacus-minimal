test: 
   pytest .
   mypy .
   python readme.py

fix:    
   isort . --float-to-top
   black .
   ruff check . --fix
