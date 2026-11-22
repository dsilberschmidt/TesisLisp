#!/usr/bin/env python3
# Oraculo independiente para run-tests.sh: uso python3 oraculo.py
# Oraculo independiente: chequeo de modelos (fuerza bruta) para los problemas proposicionales
from itertools import product
X=lambda a,b:a!=b; I=lambda a,b:a==b
def rooms(m): return X(m['h1d'],m['h1t']) and X(m['h2d'],m['h2t'])
P={
 'dama1':(['h1d','h1t','h2d','h2t'],lambda m: rooms(m) and X(m['h1d'] and m['h2t'], X(m['h1d'] and m['h2t'], m['h2d'] and m['h1t']))),
 'dama2':(['h1d','h1t','h2d','h2t'],lambda m: rooms(m) and I(m['h1d'] or m['h2d'], m['h1t'])),
 'dama3':(['h1d','h1t','h2d','h2t'],lambda m: rooms(m) and I(m['h1t'] or m['h2d'], m['h1d'])),
 'dama4':(['h1d','h1t','h2d','h2t','l1','l2'],lambda m: rooms(m) and I(m['l1'],m['h1d'] and m['h2d']) and I(m['l2'],m['h1d'] and m['h2d']) and I(m['h1d'],m['l1']) and I(m['h2t'],m['l2'])),
 'dama5':(['h1d','h1t','h2d','h2t','l1','l2'],lambda m: rooms(m) and I(m['l1'],m['h1d'] or m['h2d']) and I(m['l2'],m['h1d']) and I(m['h1d'],m['l1']) and I(m['h2t'],m['l2'])),
 'dama6':(['h1d','h1t','h2d','h2t','l1','l2'],lambda m: rooms(m) and I(m['l1'],I(m['h1d'],m['h2d'])) and I(m['l1'],I(m['h1t'],m['h2t'])) and I(m['l2'],m['h1d']) and I(m['h1d'],m['l1']) and I(m['h2t'],m['l2'])),
 'dama7':(['h1d','h1t','h2d','h2t','l1','l2'],lambda m: rooms(m) and I(m['l1'],X(m['h1d'],m['h2d'])) and I(m['l1'],X(m['h1t'],m['h2t'])) and I(m['l2'],m['h1d'] and m['h2t']) and I(m['h1d'],m['l1']) and I(m['h2t'],m['l2'])),
 'presidente2':(['a','b','c'],lambda m:(m['a'] or m['b']) and (m['a'] or m['c']) and (not m['a'] or not(m['b'] or m['c'])) and (not m['b'] or not(m['a'] or m['c'])) and (not m['c'] or not(m['a'] or m['b']))),
 'transil1':(['dice','hc','hl','vc','vl'],lambda m: I(m['dice'],m['vc']) and (not m['hc'] or m['dice']) and (not m['hl'] or not m['dice']) and (not m['vc'] or not m['dice']) and (not m['vl'] or m['dice']) and X(X(m['hc'],m['vl']),X(m['vc'],m['hl']))),
 'transil2':(['dice','h','v','c','l'],lambda m: I(m['dice'],m['v'] and m['c']) and (not(m['h'] and m['c']) or m['dice']) and (not(m['h'] and m['l']) or not m['dice']) and (not(m['v'] and m['c']) or not m['dice']) and (not(m['v'] and m['l']) or m['dice']) and X(m['h'],m['v']) and X(m['c'],m['l'])),
}
for name,(at,f) in P.items():
    ms=[dict(zip(at,v)) for v in product([False,True],repeat=len(at)) if f(dict(zip(at,v)))]
    ent=[a for a in at if ms and all(m[a] for m in ms)]
    print(f"{name}: {len(ms)} modelos; implica: {ent}")
