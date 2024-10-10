test: 
   pytest .
   mypy .

fix:    
   isort . --float-to-top
   black .
   ruff check . --fix
