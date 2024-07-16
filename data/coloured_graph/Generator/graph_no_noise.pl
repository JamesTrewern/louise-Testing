:-module(graph_no_noise, [background_knowledge/2,metarules/2,positive_example/2,negative_example/2,ancestor/2,blue_parent/2,parent/2,red_parent/2]).

background_knowledge(graph_no_noise/2, [ancestor/2]).

metarules(graph_no_noise/2,[identity,inverse,switch,swap]).

positive_example(graph_no_noise/2,graph_no_noise(a,b)).
positive_example(graph_no_noise/2,graph_no_noise(a,c)).
positive_example(graph_no_noise/2,graph_no_noise(a,f)).
positive_example(graph_no_noise/2,graph_no_noise(a,g)).
positive_example(graph_no_noise/2,graph_no_noise(a,h)).
positive_example(graph_no_noise/2,graph_no_noise(a,i)).
positive_example(graph_no_noise/2,graph_no_noise(a,j)).
positive_example(graph_no_noise/2,graph_no_noise(a,k)).
positive_example(graph_no_noise/2,graph_no_noise(a,l)).
positive_example(graph_no_noise/2,graph_no_noise(a,n)).
positive_example(graph_no_noise/2,graph_no_noise(b,a)).
positive_example(graph_no_noise/2,graph_no_noise(b,c)).
positive_example(graph_no_noise/2,graph_no_noise(b,d)).
positive_example(graph_no_noise/2,graph_no_noise(b,e)).
positive_example(graph_no_noise/2,graph_no_noise(b,i)).
positive_example(graph_no_noise/2,graph_no_noise(b,j)).
positive_example(graph_no_noise/2,graph_no_noise(b,k)).
positive_example(graph_no_noise/2,graph_no_noise(b,l)).
positive_example(graph_no_noise/2,graph_no_noise(b,m)).
positive_example(graph_no_noise/2,graph_no_noise(c,a)).
positive_example(graph_no_noise/2,graph_no_noise(c,b)).
positive_example(graph_no_noise/2,graph_no_noise(c,g)).
positive_example(graph_no_noise/2,graph_no_noise(c,h)).
positive_example(graph_no_noise/2,graph_no_noise(c,i)).
positive_example(graph_no_noise/2,graph_no_noise(c,j)).
positive_example(graph_no_noise/2,graph_no_noise(c,k)).
positive_example(graph_no_noise/2,graph_no_noise(c,l)).
positive_example(graph_no_noise/2,graph_no_noise(c,n)).
positive_example(graph_no_noise/2,graph_no_noise(d,b)).
positive_example(graph_no_noise/2,graph_no_noise(d,e)).
positive_example(graph_no_noise/2,graph_no_noise(d,i)).
positive_example(graph_no_noise/2,graph_no_noise(d,j)).
positive_example(graph_no_noise/2,graph_no_noise(d,l)).
positive_example(graph_no_noise/2,graph_no_noise(d,m)).
positive_example(graph_no_noise/2,graph_no_noise(e,b)).
positive_example(graph_no_noise/2,graph_no_noise(e,d)).
positive_example(graph_no_noise/2,graph_no_noise(e,i)).
positive_example(graph_no_noise/2,graph_no_noise(e,j)).
positive_example(graph_no_noise/2,graph_no_noise(e,l)).
positive_example(graph_no_noise/2,graph_no_noise(e,m)).
positive_example(graph_no_noise/2,graph_no_noise(f,a)).
positive_example(graph_no_noise/2,graph_no_noise(f,g)).
positive_example(graph_no_noise/2,graph_no_noise(f,h)).
positive_example(graph_no_noise/2,graph_no_noise(f,k)).
positive_example(graph_no_noise/2,graph_no_noise(f,n)).
positive_example(graph_no_noise/2,graph_no_noise(g,a)).
positive_example(graph_no_noise/2,graph_no_noise(g,c)).
positive_example(graph_no_noise/2,graph_no_noise(g,f)).
positive_example(graph_no_noise/2,graph_no_noise(g,h)).
positive_example(graph_no_noise/2,graph_no_noise(g,j)).
positive_example(graph_no_noise/2,graph_no_noise(g,k)).
positive_example(graph_no_noise/2,graph_no_noise(g,n)).
positive_example(graph_no_noise/2,graph_no_noise(h,a)).
positive_example(graph_no_noise/2,graph_no_noise(h,c)).
positive_example(graph_no_noise/2,graph_no_noise(h,f)).
positive_example(graph_no_noise/2,graph_no_noise(h,g)).
positive_example(graph_no_noise/2,graph_no_noise(h,j)).
positive_example(graph_no_noise/2,graph_no_noise(h,k)).
positive_example(graph_no_noise/2,graph_no_noise(h,n)).
positive_example(graph_no_noise/2,graph_no_noise(i,a)).
positive_example(graph_no_noise/2,graph_no_noise(i,b)).
positive_example(graph_no_noise/2,graph_no_noise(i,c)).
positive_example(graph_no_noise/2,graph_no_noise(i,d)).
positive_example(graph_no_noise/2,graph_no_noise(i,e)).
positive_example(graph_no_noise/2,graph_no_noise(i,j)).
positive_example(graph_no_noise/2,graph_no_noise(i,k)).
positive_example(graph_no_noise/2,graph_no_noise(i,l)).
positive_example(graph_no_noise/2,graph_no_noise(j,a)).
positive_example(graph_no_noise/2,graph_no_noise(j,b)).
positive_example(graph_no_noise/2,graph_no_noise(j,c)).
positive_example(graph_no_noise/2,graph_no_noise(j,d)).
positive_example(graph_no_noise/2,graph_no_noise(j,e)).
positive_example(graph_no_noise/2,graph_no_noise(j,g)).
positive_example(graph_no_noise/2,graph_no_noise(j,h)).
positive_example(graph_no_noise/2,graph_no_noise(j,i)).
positive_example(graph_no_noise/2,graph_no_noise(j,k)).
positive_example(graph_no_noise/2,graph_no_noise(j,l)).
positive_example(graph_no_noise/2,graph_no_noise(j,n)).
positive_example(graph_no_noise/2,graph_no_noise(k,a)).
positive_example(graph_no_noise/2,graph_no_noise(k,b)).
positive_example(graph_no_noise/2,graph_no_noise(k,c)).
positive_example(graph_no_noise/2,graph_no_noise(k,f)).
positive_example(graph_no_noise/2,graph_no_noise(k,g)).
positive_example(graph_no_noise/2,graph_no_noise(k,h)).
positive_example(graph_no_noise/2,graph_no_noise(k,i)).
positive_example(graph_no_noise/2,graph_no_noise(k,j)).
positive_example(graph_no_noise/2,graph_no_noise(k,l)).
positive_example(graph_no_noise/2,graph_no_noise(k,n)).
positive_example(graph_no_noise/2,graph_no_noise(l,a)).
positive_example(graph_no_noise/2,graph_no_noise(l,b)).
positive_example(graph_no_noise/2,graph_no_noise(l,c)).
positive_example(graph_no_noise/2,graph_no_noise(l,d)).
positive_example(graph_no_noise/2,graph_no_noise(l,e)).
positive_example(graph_no_noise/2,graph_no_noise(l,i)).
positive_example(graph_no_noise/2,graph_no_noise(l,j)).
positive_example(graph_no_noise/2,graph_no_noise(l,k)).
positive_example(graph_no_noise/2,graph_no_noise(l,m)).
positive_example(graph_no_noise/2,graph_no_noise(m,b)).
positive_example(graph_no_noise/2,graph_no_noise(m,d)).
positive_example(graph_no_noise/2,graph_no_noise(m,e)).
positive_example(graph_no_noise/2,graph_no_noise(m,l)).
positive_example(graph_no_noise/2,graph_no_noise(n,a)).
positive_example(graph_no_noise/2,graph_no_noise(n,c)).
positive_example(graph_no_noise/2,graph_no_noise(n,f)).
positive_example(graph_no_noise/2,graph_no_noise(n,g)).
positive_example(graph_no_noise/2,graph_no_noise(n,h)).
positive_example(graph_no_noise/2,graph_no_noise(n,j)).
positive_example(graph_no_noise/2,graph_no_noise(n,k)).

negative_example(graph_no_noise/2,graph_no_noise(a,d)).
negative_example(graph_no_noise/2,graph_no_noise(a,e)).
negative_example(graph_no_noise/2,graph_no_noise(a,m)).
negative_example(graph_no_noise/2,graph_no_noise(b,f)).
negative_example(graph_no_noise/2,graph_no_noise(b,g)).
negative_example(graph_no_noise/2,graph_no_noise(b,h)).
negative_example(graph_no_noise/2,graph_no_noise(b,n)).
negative_example(graph_no_noise/2,graph_no_noise(c,d)).
negative_example(graph_no_noise/2,graph_no_noise(c,e)).
negative_example(graph_no_noise/2,graph_no_noise(c,f)).
negative_example(graph_no_noise/2,graph_no_noise(c,m)).
negative_example(graph_no_noise/2,graph_no_noise(d,a)).
negative_example(graph_no_noise/2,graph_no_noise(d,c)).
negative_example(graph_no_noise/2,graph_no_noise(d,f)).
negative_example(graph_no_noise/2,graph_no_noise(d,g)).
negative_example(graph_no_noise/2,graph_no_noise(d,h)).
negative_example(graph_no_noise/2,graph_no_noise(d,k)).
negative_example(graph_no_noise/2,graph_no_noise(d,n)).
negative_example(graph_no_noise/2,graph_no_noise(e,a)).
negative_example(graph_no_noise/2,graph_no_noise(e,c)).
negative_example(graph_no_noise/2,graph_no_noise(e,f)).
negative_example(graph_no_noise/2,graph_no_noise(e,g)).
negative_example(graph_no_noise/2,graph_no_noise(e,h)).
negative_example(graph_no_noise/2,graph_no_noise(e,k)).
negative_example(graph_no_noise/2,graph_no_noise(e,n)).
negative_example(graph_no_noise/2,graph_no_noise(f,b)).
negative_example(graph_no_noise/2,graph_no_noise(f,c)).
negative_example(graph_no_noise/2,graph_no_noise(f,d)).
negative_example(graph_no_noise/2,graph_no_noise(f,e)).
negative_example(graph_no_noise/2,graph_no_noise(f,i)).
negative_example(graph_no_noise/2,graph_no_noise(f,j)).
negative_example(graph_no_noise/2,graph_no_noise(f,l)).
negative_example(graph_no_noise/2,graph_no_noise(f,m)).
negative_example(graph_no_noise/2,graph_no_noise(g,b)).
negative_example(graph_no_noise/2,graph_no_noise(g,d)).
negative_example(graph_no_noise/2,graph_no_noise(g,e)).
negative_example(graph_no_noise/2,graph_no_noise(g,i)).
negative_example(graph_no_noise/2,graph_no_noise(g,l)).
negative_example(graph_no_noise/2,graph_no_noise(g,m)).
negative_example(graph_no_noise/2,graph_no_noise(h,b)).
negative_example(graph_no_noise/2,graph_no_noise(h,d)).
negative_example(graph_no_noise/2,graph_no_noise(h,e)).
negative_example(graph_no_noise/2,graph_no_noise(h,i)).
negative_example(graph_no_noise/2,graph_no_noise(h,l)).
negative_example(graph_no_noise/2,graph_no_noise(h,m)).
negative_example(graph_no_noise/2,graph_no_noise(i,f)).
negative_example(graph_no_noise/2,graph_no_noise(i,g)).
negative_example(graph_no_noise/2,graph_no_noise(i,h)).
negative_example(graph_no_noise/2,graph_no_noise(i,m)).
negative_example(graph_no_noise/2,graph_no_noise(i,n)).
negative_example(graph_no_noise/2,graph_no_noise(j,f)).
negative_example(graph_no_noise/2,graph_no_noise(j,m)).
negative_example(graph_no_noise/2,graph_no_noise(k,d)).
negative_example(graph_no_noise/2,graph_no_noise(k,e)).
negative_example(graph_no_noise/2,graph_no_noise(k,m)).
negative_example(graph_no_noise/2,graph_no_noise(l,f)).
negative_example(graph_no_noise/2,graph_no_noise(l,g)).
negative_example(graph_no_noise/2,graph_no_noise(l,h)).
negative_example(graph_no_noise/2,graph_no_noise(l,n)).
negative_example(graph_no_noise/2,graph_no_noise(m,a)).
negative_example(graph_no_noise/2,graph_no_noise(m,c)).
negative_example(graph_no_noise/2,graph_no_noise(m,f)).
negative_example(graph_no_noise/2,graph_no_noise(m,g)).
negative_example(graph_no_noise/2,graph_no_noise(m,h)).
negative_example(graph_no_noise/2,graph_no_noise(m,i)).
negative_example(graph_no_noise/2,graph_no_noise(m,j)).
negative_example(graph_no_noise/2,graph_no_noise(m,k)).
negative_example(graph_no_noise/2,graph_no_noise(m,n)).
negative_example(graph_no_noise/2,graph_no_noise(n,b)).
negative_example(graph_no_noise/2,graph_no_noise(n,d)).
negative_example(graph_no_noise/2,graph_no_noise(n,e)).
negative_example(graph_no_noise/2,graph_no_noise(n,i)).
negative_example(graph_no_noise/2,graph_no_noise(n,l)).
negative_example(graph_no_noise/2,graph_no_noise(n,m)).

ancestor(A,B):-parent(A,B).
ancestor(C,D):-parent(C,E),ancestor(E,D).

parent(A,B):-blue_parent(A,B).
parent(C,D):-red_parent(C,D).

blue_parent(a,c).
blue_parent(a,n).
blue_parent(b,i).
blue_parent(b,d).
blue_parent(c,j).
blue_parent(d,e).
blue_parent(f,g).
blue_parent(f,h).

red_parent(k,c).
red_parent(k,n).
red_parent(l,i).
red_parent(l,d).
red_parent(i,j).
red_parent(m,e).
red_parent(n,g).
red_parent(n,h).

