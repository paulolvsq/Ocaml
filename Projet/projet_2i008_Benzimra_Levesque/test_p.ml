let main =
  let robot={pos=(0,0); dir=Droite} in
  let grille0=[(0, [(0,Vert)])] in
  let grille1=[(0,[(0,None); (1,None); (2,None); (3,None); (4,None); (5,None)])] in
  let grille2=[(0 , [(0,Rouge); (6,Bleu); (7,Bleu)]);
          (1 , [(5,Bleu); (6,Bleu)]);
          (2 , [(4,Bleu); (5,Bleu)]);
          (3 , [(3,Bleu); (4,Bleu)]);
          (4 , [(2,Bleu); (3,Bleu)]);
          (5 , [(1,Bleu); (2,Bleu)]);
          (6 , [(0,Bleu); (1,Bleu)]);
          (7 , [(0,Vert)]);
        (10 ,[(10,Rouge)])] in
  let fonctions = [] in
  let etoiles = [(0,0)] in
  let niveau = {grille=grille2; robot=robot; fonctions=fonctions; etoiles=etoiles} in
  let pile = [(None,Avancer);(None,RotGauche);(None,RotDroite);(None,Avancer);
       (Jaune,Colorie(Vert));(None,Appel("f1"));
       (None,Avancer);(None,RotGauche);(None,RotDroite);(Rouge,Avancer)] in
  auto_synchronize false;
  init niveau (800,800);
  dessine_niveau niveau;
  synchronize ();
  while true do () done
