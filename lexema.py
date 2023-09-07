import ply.lex as lex

reserved = {
  'int' : 'TYPE_INT',
  'float' : 'TYPE_FLOAT',
  'bool' : 'TYPE_BOOL',
  'char' : 'TYPE_CHAR',
  'print' : 'PRINT',
  'read' : 'READ',
  'return' : 'RETURN',
  'for' : 'FOR',
  'while' : 'WHILE',
  'if' : 'IF',
  'else' : 'ELSE'
}

tokens = ['NUMBER','PLUS','MINUS','TIMES','DIVIDE','LPAREN','RPAREN','ID','DECM','COMMET_SIM','STRING','CHAR']+list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_STRING = r'".*"'
t_CHAR = r"'.'"
##t_ID = r'([a-zA-Z]|\_)([a-zA-Z]|[0-9]|\_)*'
###t_DECM = r'\d+\.\d+'
### A regular expression rule with some action code
def t_DECM(t):
  r'\d+\.\d+'
  t.value = float(t.value)
  return t
'''def CHAR(t):
  r'"\w"'
  t.type = str(t.value)
  return t'''
def t_ID(t):
  r'([a-zA-Z]|\_)([a-zA-Z]|[0-9]|\_)*'
  t.type = reserved.get(t.value,'ID')
  return t
def t_NUMBER(t):
  r'\d+'
  t.value = int(t.value) # guardamos el valor del lexema
  return t
  # Define a rule so we can track line numbers
def t_newline(t):
  r'\n+'
  t.lexer.lineno += len(t.value)
# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'
# Error handling rule
def t_error(t):
  print("Illegal character '%s'" % t.value[0])
  t.lexer.skip(1)
# Build the lexer
lexer = lex.lex()
# Test it out
data = '''if else for while return'''
# Give the lexer some input
lexer.input(data)
# Tokenize
while True:
  tok = lexer.token()
  if not tok:
    break # No more input
  print(tok)
  #print(tok.type, tok.value, tok.lineno, tok.lexpos)