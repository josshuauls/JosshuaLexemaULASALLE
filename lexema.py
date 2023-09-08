import ply.lex as lex

reserved = {
  'kancha' : 'TYPE_INT',
  'lliqlla' : 'TYPE_FLOAT',
  'wañuchiy' : 'TYPE_BOOL',
  'wakchaq' : 'TYPE_CHAR',
  'quilway' : 'PRINT',
  'ñawi' : 'READ',
  'qillqay' : 'RETURN',
  'maypi' : 'FOR',
  'hamuy' : 'WHILE',
  'kunan' : 'IF',
  'chaypi' : 'ELSE'
}

tokens = ['NUMBER','DECM','ID',
          'OPER_ASIG','OPER_ADD','OPER_SUB','OPER_MUL','OPER_DIV','OPER_MOD','OPER_AND','OPER_OR',
          'OPER_MYQ','OPER_MNQ','OPER_MYI','OPER_MNI','OPER_IQL','OPER_DIF',
          'PARENT_OP','PARENT_CL','KEY_OP','KEY_CL','SQUARE_OP','SQUARE_CL',
          'COMMET_SIM','COMMET_COM',
          'STRING','CHAR']+list(reserved.values())

t_OPER_ASIG = r'\='
t_OPER_ADD = r'\+'
t_OPER_SUB = r'-'
t_OPER_MUL = r'\*'
t_OPER_DIV = r'/'
t_OPER_MOD = r'\%'
t_OPER_AND = r'\&'
t_OPER_OR = r'\|'

t_OPER_MYQ = r'>'
t_OPER_MNQ = r'<'
t_OPER_MYI = r'>='
t_OPER_MNI = r'<='
t_OPER_IQL = r'=='
t_OPER_DIF = r'!='

t_PARENT_OP = r'\('
t_PARENT_CL = r'\)'
t_KEY_OP = r'\{'
t_KEY_CL = r'\}'
t_SQUARE_OP = r'\['
t_SQUARE_CL = r'\]'

t_COMMET_SIM = r'//.*'
t_COMMET_COM = r'///.*///'
t_STRING = r'".*"'
t_CHAR = r"'.'"


### A regular expression rule with some action code
def t_DECM(t):
  r'\d+\.\d+'
  t.value = float(t.value)
  return t
def t_ID(t):
  r'([a-zA-Z]|ñ|Ñ|\_)([a-zA-Z]|[0-9]|ñ|Ñ|\_)*'
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


data = ''' maypi hamuy kunan chaypi kancha lliqlla wañuchiy wakchaq quilway ñawi qillqay'''
# Give the lexer some input
lexer.input(data)
# Tokenize
while True:
  tok = lexer.token()
  if not tok:
    break # No more input
  print(tok)
  #print(tok.type, tok.value, tok.lineno, tok.lexpos)