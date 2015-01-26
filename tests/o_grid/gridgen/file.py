'''
Created on 06.12.2013

@author: roman
'''
from block import block 
from viereck import viereck
from kreiseck import kreiseck
from ellipseck2 import ellipseck2
from ellipseck import ellipseck
import math
import numpy
# import block_drehen
def grid():
    print "DAS IST DER OGRID TESFALL 08.10.2014"
    ############## PARAMETER DES GRIDS ###################################
        ########## GITTER ZELLEN ANZAHL
    # RADIAL
    nr1 = 16*2
    nr2 = 16*2
    nr3 = 16*2  
    nr4 = 16*2
    nr5 = 64*2
    sum_r = nr1+nr2+nr3+nr4+nr5
    #AXIAL
    ni1 = 16*2          # einspritzung
    ni2 = 278           # brennkammer
    ni3 = 30            # konvergente duesenteil
    ni4 = 2             # flaches Duesenhalsstueck
    ni5 = 10            # divergenter duesenteil
    
    
    
    ni2 = 299           # brennkammer
    ni3 = 15            # konvergente duesenteil
    ni4 = 1             # flaches Duesenhalsstueck
    ni5 = 5            # divergenter duesenteil
    
    ni21 = 60           # Zwischen ebene 
    ni22 = ni2-ni21
    ##########DIMENSIONEN
    #### BRENNKAMMER
    #LAENGE
    dim_l = 290.*0.001
    dim_l_zw = 0.010125608        # Zwischenebene
    #HOEHE
    dim_h = 6.*0.001
    #TIEFE
    dim_t = 6.*0.001
    ##### EINLASS (INLET)
    #LAENGE
    dim_l_I = 10.*0.001
    #RADIUS # o2 EINLASS
    dim_r_I = 2.*0.001
    # POSITION DES O.GRID ZENTERPKT
    r2 = 1.11*0.001
    # POSITION DER OGRID KANTE
    r1 = 1.577*0.001    

    #INNENRADIUS CH4
    dim_r1_I = 2.5*0.001
    #AUSENRADIUS CH4
    dim_r2_I = 3.*0.001
    
    #### DUESE (THROAT)
    dim_h_T  = 2.4*0.001
    dim_h_e  = 3.8 * 0.001
    
    dim_l_konv = 13.44*0.001
    dim_l_flach = 1*0.001
    dim_l_div = 5.56*0.001
    
    dim_l_T = dim_l + dim_l_konv
    dim_l_T2 = dim_l_T + dim_l_flach
    dim_l_T3 = dim_l_T2 + dim_l_div
    
    fa = 1.1
    fa2 = 1.1
    dim_t1 = fa*(1.0*nr1)/(1.0*sum_r) * dim_h_T
    dim_t2 = fa*(1.0*nr1)/(1.0*sum_r) * dim_h_T * r2/r1
    dim_t3 = fa*(1.0*(nr1+nr2))/(1.0*sum_r) * dim_h_T
    dim_t4 = fa*(1.0*(nr1+nr2+nr3))/(1.0*sum_r) * dim_h_T
    dim_t5 = fa*(1.0*(nr1+nr2+nr3+nr4))/(1.0*sum_r) * dim_h_T
    
    dim_t1s = fa2*dim_t1 * dim_h / dim_h_T
    dim_t2s = fa2*dim_t2 * dim_h / dim_h_T
    dim_t3s = fa2*dim_t3 * dim_h / dim_h_T
    dim_t4s = fa2*dim_t4 * dim_h / dim_h_T
    dim_t5s = fa2*dim_t5 * dim_h / dim_h_T
    
    dim_t1e = fa2*dim_t1 * dim_h_e / dim_h_T
    dim_t2e = fa2*dim_t2 * dim_h_e / dim_h_T
    dim_t3e = fa2*dim_t3 * dim_h_e / dim_h_T
    dim_t4e = fa2*dim_t4 * dim_h_e / dim_h_T
    dim_t5e = fa2*dim_t5 * dim_h_e / dim_h_T
    
#     print dim_t1,dim_t2,dim_t3,dim_t4,dim_t5,dim_h_T
#     print dim_t1s,dim_t2s,dim_t3s,dim_t4s,dim_t5s,dim_h
#     print r1,r2,dim_r_I,dim_r1_I,dim_r2_I,dim_h
    
    f1 = 0.131225931
    f2 = 0.278510854
    f3 = 0.425795776
    f4 = 0.573080699

    P1          = dim_h * f1
    P2          = dim_h * f2
    P3          = dim_h * f3
    P4          = dim_h * f4 
    
    P1_throat   = dim_h_T * f1
    P2_throat   = dim_h_T * f2
    P3_throat   = dim_h_T * f3
    P4_throat   = dim_h_T * f4
    
    P1_exit     = dim_h_e * f1
    P2_exit     = dim_h_e * f2
    P3_exit     = dim_h_e * f3
    P4_exit     = dim_h_e * f4
       
    dn_WAND     = 0.001*0.001
    
    dn_uebergang_o_grid = -1 #0.02*0.001
    
    dn_einstr = dn_WAND * 1.0
    
    dn_in = 0.0005 # 0.002199159

    
    dn_uebergang_ideal_o_grid = 2.76159E-05

    dn_zwischen = -1 #0.001283934
   
    print dn_WAND
    blocks = []
    
# FRONTALANSICHT:
# Y
# ^
# |   ____________________
# |  |                  /|
# |  |_______  8      /  | 
# |  |  6     \     /    |
# |  |______   \  /      |
# |  |  4    \  /     9  |
# |  |______  \/  \      |
# |  |  2   \/  \  \     |
# |  |_____ /\   \  \    |
# |  |     |  \   |  |   |
# |  |  1  | 3 |5 |7 |   |
# |  |_____|___|__|__|___|
# |_____________________________> Z
        
        
# SEITANSICHT:


# Y
# ^
# |      ___________________________________________________
# |      |                                                 |\
# |      |                                                 | \
# |      |                                                 |  \     /|
# | _____|_________________________________________________|_  \   / |
# | |    |                                                 |  \ \_/ /|
# | |____|_________________________________________________|__ \|_|//|
# | _____|_________________________________________________|_  \|_|/ |
# | |    |                                                 |  \_|_|_/|
# | |____|_________________________________________________|____|_|__|
# | |    |                                                 |    | |  |
# | |____|_________________________________________________|____|_|__|
# |_______________________________________________________________________> X    
    
    
#################INLET###################
################ O2

    RW  = numpy.zeros((3,4))
    RW1 = numpy.zeros((3,4))
    RW2 = numpy.zeros((3,4))
    RW3 = numpy.zeros((3,4))
    
    RW[0][0] = -1
    RW[0][1] = dn_uebergang_o_grid
    RW[0][2] = dn_uebergang_o_grid
    RW[0][3] = -1
     
    RW[1][0] = -1
    RW[1][1] = -1
    RW[1][2] = dn_uebergang_o_grid
    RW[1][3] = dn_uebergang_o_grid
 
    RW[2][0] = dn_einstr * 20.0
    RW[2][1] = dn_einstr * 5.0
    RW[2][2] = dn_einstr * 5.0
    RW[2][3] = dn_einstr * 5.0
     
    RW1[0][0] = dn_uebergang_o_grid
    RW1[0][1] = dn_WAND
    RW1[0][2] = dn_WAND
    RW1[0][3] = dn_uebergang_o_grid
    
    RW1[1][0] = -1
    RW1[1][1] = -1
    RW1[1][2] = -1
    RW1[1][3] = dn_uebergang_o_grid
 
    RW1[2][0] = dn_einstr * 5.0
    RW1[2][1] = dn_einstr * 5.0
    RW1[2][2] = dn_einstr
    RW1[2][3] = dn_einstr
    
    RW2[0][0] = dn_uebergang_o_grid
    RW2[0][1] = dn_WAND
    RW2[0][2] = dn_WAND
    RW2[0][3] = dn_uebergang_o_grid
     
    RW2[1][0] = dn_uebergang_o_grid
    RW2[1][1] = -1
    RW2[1][2] = -1
    RW2[1][3] = -1

    RW2[2][0] = dn_einstr * 5.0
    RW2[2][1] = dn_einstr * 5.0
    RW2[2][2] = dn_einstr
    RW2[2][3] = dn_einstr
    
    RW3[0][0] = dn_WAND
    RW3[0][1] = dn_WAND
    RW3[0][2] = dn_WAND
    RW3[0][3] = dn_WAND
    
    RW3[1][0] = -1
    RW3[1][1] = -1
    RW3[1][2] = -1
    RW3[1][3] = -1
    
    RW3[2][0] = dn_einstr
    RW3[2][1] = dn_einstr
    RW3[2][2] = dn_einstr
    RW3[2][3] = dn_einstr

    RW_in  = numpy.zeros((3,4))
    RW1_in = numpy.zeros((3,4))
    RW2_in = numpy.zeros((3,4))
    RW3_in = numpy.zeros((3,4))
    
    RW_in[0][0] = -1
    RW_in[0][1] = dn_uebergang_o_grid
    RW_in[0][2] = dn_uebergang_o_grid
    RW_in[0][3] = -1
     
    RW_in[1][0] = -1
    RW_in[1][1] = -1
    RW_in[1][2] = dn_uebergang_o_grid
    RW_in[1][3] = dn_uebergang_o_grid
 
    RW_in[2][0] = dn_in
    RW_in[2][1] = dn_in
    RW_in[2][2] = dn_in
    RW_in[2][3] = dn_in
     
    RW1_in[0][0] = dn_uebergang_o_grid
    RW1_in[0][1] = dn_WAND
    RW1_in[0][2] = dn_WAND
    RW1_in[0][3] = dn_uebergang_o_grid
    
    RW1_in[1][0] = -1
    RW1_in[1][1] = -1
    RW1_in[1][2] = -1
    RW1_in[1][3] = dn_uebergang_o_grid
 
    RW1_in[2][0] = dn_in
    RW1_in[2][1] = dn_in
    RW1_in[2][2] = dn_in
    RW1_in[2][3] = dn_in
    

    RW2_in[0][0] = dn_uebergang_o_grid
    RW2_in[0][1] = dn_WAND
    RW2_in[0][2] = dn_WAND
    RW2_in[0][3] = dn_uebergang_o_grid
     
    RW2_in[1][0] = dn_uebergang_o_grid
    RW2_in[1][1] = -1
    RW2_in[1][2] = -1
    RW2_in[1][3] = -1

    RW2_in[2][0] = dn_in
    RW2_in[2][1] = dn_in
    RW2_in[2][2] = dn_in
    RW2_in[2][3] = dn_in

    
    RW3_in[0][0] = dn_WAND
    RW3_in[0][1] = dn_WAND
    RW3_in[0][2] = dn_WAND
    RW3_in[0][3] = dn_WAND
    
    RW3_in[1][0] = -1
    RW3_in[1][1] = -1
    RW3_in[1][2] = -1
    RW3_in[1][3] = -1
    
    RW3_in[2][0] = dn_in
    RW3_in[2][1] = dn_in
    RW3_in[2][2] = dn_in
    RW3_in[2][3] = dn_in
        
########################### 1 ############################        
    blocks.append(block(nr1,nr1,ni1))  
                                  
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_in, \
                            [-dim_l_I,0.,0.], \
                            [-dim_l_I,r1,0.], \
                            [-dim_l_I,r2,r2], \
                            [-dim_l_I,0.,r1]) ) 
          
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW, \
                            [0.,0.,0.], \
                            [0.,r1,0.], \
                            [0.,r2,r2], \
                            [0.,0.,r1]) )                                
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 2 ############################      
    blocks.append(block(nr2,nr1,ni1)) 
          
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW1_in, \
                            [-dim_l_I,r1,0.], \
                            [-dim_l_I,dim_r_I,0.], \
                            [-dim_l_I,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [-dim_l_I,r2,r2],\
                            0., dim_r_I )) 
          
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW1, \
                            [0.,r1,0.], \
                            [0.,dim_r_I,0.], \
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [0.,r2,r2],\
                            0., dim_r_I ))    
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
       
########################### 3 ############################       
    blocks.append(block(nr2,nr1,ni1))
           
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW2_in, \
                            [-dim_l_I,r2,r2],\
                            [-dim_l_I,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [-dim_l_I,0.,dim_r_I], \
                            [-dim_l_I,0.,r1], \
                            0., dim_r_I ))  
          
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW2, \
                            [0.,r2,r2],\
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [0.,0.,dim_r_I], \
                            [0.,0.,r1], \
                            0., dim_r_I ))     
         
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    return blocks
########################### 6 ############################  
    blocks.append(block(nr4,nr1,ni1))
          
    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3_in, \
                            [-dim_l_I,dim_r1_I,0.], \
                            [-dim_l_I,dim_r2_I,0.], \
                            [-dim_l_I,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [-dim_l_I,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)], \
                            dim_r1_I , dim_r2_I ) )  
         
    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3, \
                            [0.,dim_r1_I,0.], \
                            [0.,dim_r2_I,0.], \
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)], \
                            dim_r1_I , dim_r2_I ) ) 
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni1))

    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3_in, \
                            [-dim_l_I,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)],\
                            [-dim_l_I,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [-dim_l_I,0.,dim_r2_I], \
                            [-dim_l_I,0.,dim_r1_I], \
                            dim_r1_I,dim_r2_I ))
         
    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3, \
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)],\
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [0.,0.,dim_r2_I], \
                            [0.,0.,dim_r1_I], \
                            dim_r1_I,dim_r2_I ))     
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
       
################# BRENNKAMMER Erster Teil bis Zwischenteil
        
    RW_ideal_ogrid  = numpy.zeros((3,4))
 
    RW_ideal_ogrid[0][0] = -1
    RW_ideal_ogrid[0][1] = -1
    RW_ideal_ogrid[0][2] = -1
    RW_ideal_ogrid[0][3] = -1
      
    RW_ideal_ogrid[1][0] = -1
    RW_ideal_ogrid[1][1] = -1
    RW_ideal_ogrid[1][2] = -1
    RW_ideal_ogrid[1][3] = -1
  
    RW_ideal_ogrid[2][0] = dn_zwischen
    RW_ideal_ogrid[2][1] = dn_zwischen
    RW_ideal_ogrid[2][2] = dn_zwischen
    RW_ideal_ogrid[2][3] = dn_zwischen
 
 
    RW_ideal_ogrid_aussen  = numpy.zeros((3,4))
 
    RW_ideal_ogrid_aussen[0][0] = dn_uebergang_ideal_o_grid
    RW_ideal_ogrid_aussen[0][1] = dn_WAND
    RW_ideal_ogrid_aussen[0][2] = dn_WAND
    RW_ideal_ogrid_aussen[0][3] = dn_uebergang_ideal_o_grid
      
    RW_ideal_ogrid_aussen[1][0] = -1
    RW_ideal_ogrid_aussen[1][1] = -1
    RW_ideal_ogrid_aussen[1][2] = -1
    RW_ideal_ogrid_aussen[1][3] = -1
  
    RW_ideal_ogrid_aussen[2][0] = dn_zwischen
    RW_ideal_ogrid_aussen[2][1] = dn_zwischen
    RW_ideal_ogrid_aussen[2][2] = dn_zwischen
    RW_ideal_ogrid_aussen[2][3] = dn_zwischen
   
########################### 1 ############################
    blocks.append(block(nr1,nr1,ni21))
               
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW, \
                            [0.,0.,0.], \
                            [0.,r1,0.], \
                            [0.,r2,r2], \
                            [0.,0.,r1]) )   
                                   
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,0.,0.], \
                            [dim_l_zw,P1,0.], \
                            [dim_l_zw,P1,P1], \
                            [dim_l_zw,0.,P1]) )                                
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
      
########################### 2 ############################      
    blocks.append(block(nr2,nr1,ni21))
             
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW1, \
                            [0.,r1,0.], \
                            [0.,dim_r_I,0.], \
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [0.,r2,r2],\
                            0., dim_r_I ))     
             
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P1,0.], \
                            [dim_l_zw,P2,0.], \
                            [dim_l_zw,P2,P2], \
                            [dim_l_zw,P1,P1] )) 
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
      
########################### 3 ############################       
    blocks.append(block(nr2,nr1,ni21))
             
    blocks[-1].Ebenen.append(kreiseck(nr2,nr1, \
                            RW2, \
                            [0.,r2,r2],\
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            [0.,0.,dim_r_I], \
                            [0.,0.,r1], \
                            0., dim_r_I ))     
              
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P1,P1],\
                            [dim_l_zw,P2,P2], \
                            [dim_l_zw,0.,P2], \
                            [dim_l_zw,0.,P1] ))  
            
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
          
########################### 4 ############################
    blocks.append(block(nr3,nr1,ni21))
             
    blocks[-1].Ebenen.append(kreiseck(nr3,nr1, \
                            RW3, \
                            [0.,dim_r_I,0.], \
                            [0.,dim_r1_I,0.], \
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)], \
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)], \
                            dim_r_I , dim_r1_I ) )     
             
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P2,0.], \
                            [dim_l_zw,P3,0.], \
                            [dim_l_zw,P3,P3], \
                            [dim_l_zw,P2,P2] ) ) 
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
      
########################### 5 ############################       
    blocks.append(block(nr3,nr1,ni21))
              
    blocks[-1].Ebenen.append(kreiseck(nr3,nr1, \
                            RW3, \
                            [0.,dim_r_I/math.sqrt(2.),dim_r_I/math.sqrt(2.)],\
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)], \
                            [0.,0.,dim_r1_I], \
                            [0.,0.,dim_r_I], \
                            dim_r_I,dim_r1_I ))     
               
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P2,P2],\
                            [dim_l_zw,P3,P3], \
                            [dim_l_zw,0.,P3], \
                            [dim_l_zw,0.,P2] )) 
             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)  
      
########################### 6 ############################        
    blocks.append(block(nr4,nr1,ni21))
             
    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3, \
                            [0.,dim_r1_I,0.], \
                            [0.,dim_r2_I,0.], \
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)], \
                            dim_r1_I , dim_r2_I ) )     
             
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P3,0.], \
                            [dim_l_zw,P4,0.], \
                            [dim_l_zw,P4,P4], \
                            [dim_l_zw,P3,P3] ) )  
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
      
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni21))
              
    blocks[-1].Ebenen.append(kreiseck(nr4,nr1, \
                            RW3, \
                            [0.,dim_r1_I/math.sqrt(2.),dim_r1_I/math.sqrt(2.)],\
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            [0.,0.,dim_r2_I], \
                            [0.,0.,dim_r1_I], \
                            dim_r1_I,dim_r2_I ))     
               
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P3,P3],\
                            [dim_l_zw,P4,P4], \
                            [dim_l_zw,0.,P4], \
                            [dim_l_zw,0.,P3]))
             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
     
########################### 8 ############################      
    blocks.append(block(nr5,nr1,ni21))
             
    blocks[-1].Ebenen.append(kreiseck(nr5,nr1, \
                            RW3, \
                            [0.,dim_r2_I,0.], \
                            [0.,dim_h,0.], \
                            [0.,dim_h,dim_h], \
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)], \
                            dim_r2_I, 0. ) )     
             
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l_zw,P4,0.], \
                            [dim_l_zw,dim_h,0.], \
                            [dim_l_zw,dim_h,dim_h], \
                            [dim_l_zw,P4,P4] ) )     
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
            
########################### 9 ############################       
    blocks.append(block(nr5,nr1,ni21))
              
    blocks[-1].Ebenen.append(kreiseck(nr5,nr1, \
                            RW3, \
                            [0.,dim_r2_I/math.sqrt(2.),dim_r2_I/math.sqrt(2.)],\
                            [0.,dim_h,dim_h], \
                            [0.,0.,dim_h], \
                            [0.,0.,dim_r2_I], \
                            dim_r2_I, 0. ))     
               
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l_zw,P4,P4],\
                            [dim_l_zw,dim_t,dim_t], \
                            [dim_l_zw,0.,dim_t], \
                            [dim_l_zw,0.,P4]))
             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
 
#
#
#
################# BRENNKAMMER Zweiter Teil ab Zwischenteil bis Duese
## 
       
 
########################### 1 ############################
    blocks.append(block(nr1,nr1,ni22))
              
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,0.,0.], \
                            [dim_l_zw,P1,0.], \
                            [dim_l_zw,P1,P1], \
                            [dim_l_zw,0.,P1]) )    
                                     
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,0.,0.], \
                            [dim_l,P1,0.], \
                            [dim_l,P1,P1], \
                            [dim_l,0.,P1]) )                                
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    
########################### 2 ############################      
    blocks.append(block(nr2,nr1,ni22))
              
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P1,0.], \
                            [dim_l_zw,P2,0.], \
                            [dim_l_zw,P2,P2], \
                            [dim_l_zw,P1,P1] )) 
               
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P1,0.], \
                            [dim_l,P2,0.], \
                            [dim_l,P2,P2], \
                            [dim_l,P1,P1] )) 
      
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    
########################### 3 ############################       
    blocks.append(block(nr2,nr1,ni22))
           
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P1,P1],\
                            [dim_l_zw,P2,P2], \
                            [dim_l_zw,0.,P2], \
                            [dim_l_zw,0.,P1] )) 
                 
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P1,P1],\
                            [dim_l,P2,P2], \
                            [dim_l,0.,P2], \
                            [dim_l,0.,P1] ))  
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
        
########################### 4 ############################
    blocks.append(block(nr3,nr1,ni22))
     
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P2,0.], \
                            [dim_l_zw,P3,0.], \
                            [dim_l_zw,P3,P3], \
                            [dim_l_zw,P2,P2] ) ) 
             
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P2,0.], \
                            [dim_l,P3,0.], \
                            [dim_l,P3,P3], \
                            [dim_l,P2,P2] ) ) 
      
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    
########################### 5 ############################       
    blocks.append(block(nr3,nr1,ni22))
     
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P2,P2],\
                            [dim_l_zw,P3,P3], \
                            [dim_l_zw,0.,P3], \
                            [dim_l_zw,0.,P2] )) 
                 
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P2,P2],\
                            [dim_l,P3,P3], \
                            [dim_l,0.,P3], \
                            [dim_l,0.,P2] )) 
           
    blocks[-1].make_grid()
    blocks[-1].rotate(1)  
    
########################### 6 ############################        
    blocks.append(block(nr4,nr1,ni22))
           
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P3,0.], \
                            [dim_l_zw,P4,0.], \
                            [dim_l_zw,P4,P4], \
                            [dim_l_zw,P3,P3] ) )     
           
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P3,0.], \
                            [dim_l,P4,0.], \
                            [dim_l,P4,P4], \
                            [dim_l,P3,P3] ) )  
      
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni22))
            
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_zw,P3,P3],\
                            [dim_l_zw,P4,P4], \
                            [dim_l_zw,0.,P4], \
                            [dim_l_zw,0.,P3]))
     
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P3,P3],\
                            [dim_l,P4,P4], \
                            [dim_l,0.,P4], \
                            [dim_l,0.,P3]))
           
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
    
########################### 8 ############################      
    blocks.append(block(nr5,nr1,ni22))  
     
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l_zw,P4,0.], \
                            [dim_l_zw,dim_h,0.], \
                            [dim_l_zw,dim_h,dim_h], \
                            [dim_l_zw,P4,P4] ) )   
             
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l,P4,0.], \
                            [dim_l,dim_h,0.], \
                            [dim_l,dim_h,dim_h], \
                            [dim_l,P4,P4] ) )     
       
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
           
########################### 9 ############################       
    blocks.append(block(nr5,nr1,ni22))
             
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l_zw,P4,P4],\
                            [dim_l_zw,dim_t,dim_t], \
                            [dim_l_zw,0.,dim_t], \
                            [dim_l_zw,0.,P4]))
              
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l,P4,P4],\
                            [dim_l,dim_t,dim_t], \
                            [dim_l,0.,dim_t], \
                            [dim_l,0.,P4]))
            
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
 
       



################# DUESE
        
#
#                        konvergierender teil
#
 
    RW_ideal_ogrid_aussen_throat  = numpy.zeros((3,4))
 
    RW_ideal_ogrid_aussen_throat[0][0] = 2.76159E-05
    RW_ideal_ogrid_aussen_throat[0][1] = dn_WAND
    RW_ideal_ogrid_aussen_throat[0][2] = dn_WAND
    RW_ideal_ogrid_aussen_throat[0][3] = 2.76159E-05
 
      
    RW_ideal_ogrid_aussen_throat[1][0] = -1
    RW_ideal_ogrid_aussen_throat[1][1] = -1
    RW_ideal_ogrid_aussen_throat[1][2] = -1
    RW_ideal_ogrid_aussen_throat[1][3] = -1
  
    RW_ideal_ogrid_aussen_throat[2][0] = -1
    RW_ideal_ogrid_aussen_throat[2][1] = -1
    RW_ideal_ogrid_aussen_throat[2][2] = -1
    RW_ideal_ogrid_aussen_throat[2][3] = -1
     
    RW_ideal_ogrid_aussen_oben_throat  = numpy.zeros((3,4))
 
    RW_ideal_ogrid_aussen_oben_throat[0][0] = 2.76159E-05 / 3.
    RW_ideal_ogrid_aussen_oben_throat[0][1] = dn_WAND / 3.
    RW_ideal_ogrid_aussen_oben_throat[0][2] = dn_WAND
    RW_ideal_ogrid_aussen_oben_throat[0][3] = 2.76159E-05
 
      
    RW_ideal_ogrid_aussen_oben_throat[1][0] = -1
    RW_ideal_ogrid_aussen_oben_throat[1][1] = -1
    RW_ideal_ogrid_aussen_oben_throat[1][2] = -1
    RW_ideal_ogrid_aussen_oben_throat[1][3] = -1
  
    RW_ideal_ogrid_aussen_oben_throat[2][0] = -1
    RW_ideal_ogrid_aussen_oben_throat[2][1] = -1
    RW_ideal_ogrid_aussen_oben_throat[2][2] = -1
    RW_ideal_ogrid_aussen_oben_throat[2][3] = -1
     
    RW_ideal_ogrid_aussen_exit  = numpy.zeros((3,4))
 
    RW_ideal_ogrid_aussen_exit[0][0] = 2.76159E-05
    RW_ideal_ogrid_aussen_exit[0][1] = dn_WAND
    RW_ideal_ogrid_aussen_exit[0][2] = dn_WAND
    RW_ideal_ogrid_aussen_exit[0][3] = 2.76159E-05
 
      
    RW_ideal_ogrid_aussen_exit[1][0] = -1
    RW_ideal_ogrid_aussen_exit[1][1] = -1
    RW_ideal_ogrid_aussen_exit[1][2] = -1
    RW_ideal_ogrid_aussen_exit[1][3] = -1
  
    RW_ideal_ogrid_aussen_exit[2][0] = -1
    RW_ideal_ogrid_aussen_exit[2][1] = -1
    RW_ideal_ogrid_aussen_exit[2][2] = -1
    RW_ideal_ogrid_aussen_exit[2][3] = -1
     
    RW_ideal_ogrid_aussen_oben_exit  = numpy.zeros((3,4))
 
    RW_ideal_ogrid_aussen_oben_exit[0][0] = 2.76159E-05 / 3.
    RW_ideal_ogrid_aussen_oben_exit[0][1] = dn_WAND / 3.
    RW_ideal_ogrid_aussen_oben_exit[0][2] = dn_WAND
    RW_ideal_ogrid_aussen_oben_exit[0][3] = 2.76159E-05
 
      
    RW_ideal_ogrid_aussen_oben_exit[1][0] = -1
    RW_ideal_ogrid_aussen_oben_exit[1][1] = -1
    RW_ideal_ogrid_aussen_oben_exit[1][2] = -1
    RW_ideal_ogrid_aussen_oben_exit[1][3] = -1
  
    RW_ideal_ogrid_aussen_oben_exit[2][0] = -1
    RW_ideal_ogrid_aussen_oben_exit[2][1] = -1
    RW_ideal_ogrid_aussen_oben_exit[2][2] = -1
    RW_ideal_ogrid_aussen_oben_exit[2][3] = -1
     
     
########################### 1 ############################
    blocks.append(block(nr1,nr1,ni3))
                
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,0.,0.], \
                            [dim_l,P1,0.], \
                            [dim_l,P1,P1], \
                            [dim_l,0.,P1]) )   
       
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,0.,0.], \
                            [dim_l_T,P1_throat,0.], \
                            [dim_l_T,P1_throat,P1], \
                            [dim_l_T,0.,P1]) )                             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 2 ############################      
    blocks.append(block(nr2,nr1,ni3))
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P1,0.], \
                            [dim_l,P2,0.], \
                            [dim_l,P2,P2], \
                            [dim_l,P1,P1] ))    
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P1_throat,0.], \
                            [dim_l_T,P2_throat,0.], \
                            [dim_l_T,P2_throat,P2], \
                            [dim_l_T,P1_throat,P1] )) 
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 3 ############################       
    blocks.append(block(nr2,nr1,ni3))
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P1,P1],\
                            [dim_l,P2,P2], \
                            [dim_l,0.,P2], \
                            [dim_l,0.,P1] ))   
          
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P1_throat,P1],\
                            [dim_l_T,P2_throat,P2], \
                            [dim_l_T,0.,P2], \
                            [dim_l_T,0.,P1] ))  
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 4 ############################  
    blocks.append(block(nr3,nr1,ni3))
          
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P2,0.], \
                            [dim_l,P3,0.], \
                            [dim_l,P3,P3], \
                            [dim_l,P2,P2] ) )  
          
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P2_throat,0.], \
                            [dim_l_T,P3_throat,0.], \
                            [dim_l_T,P3_throat,P3], \
                            [dim_l_T,P2_throat,P2] ) )  
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 5 ############################       
    blocks.append(block(nr3,nr1,ni3))
           
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P2,P2],\
                            [dim_l,P3,P3], \
                            [dim_l,0.,P3], \
                            [dim_l,0.,P2] ))   
            
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P2_throat,P2],\
                            [dim_l_T,P3_throat,P3], \
                            [dim_l_T,0.,P3], \
                            [dim_l_T,0.,P2] )) 
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1)  
   
########################### 6 ############################        
    blocks.append(block(nr4,nr1,ni3))
          
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P3,0.], \
                            [dim_l,P4,0.], \
                            [dim_l,P4,P4], \
                            [dim_l,P3,P3] ) )      
          
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P3_throat,0.], \
                            [dim_l_T,P4_throat,0.], \
                            [dim_l_T,P4_throat,P4], \
                            [dim_l_T,P3_throat,P3] ) ) 
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni3))
           
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l,P3,P3],\
                            [dim_l,P4,P4], \
                            [dim_l,0.,P4], \
                            [dim_l,0.,P3]))   
            
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P3_throat,P3],\
                            [dim_l_T,P4_throat,P4], \
                            [dim_l_T,0.,P4], \
                            [dim_l_T,0.,P3]))
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 8 ############################      
    blocks.append(block(nr5,nr1,ni3))
           
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l,P4,0.], \
                            [dim_l,dim_h,0.], \
                            [dim_l,dim_h,dim_h], \
                            [dim_l,P4,P4] ) )     
           
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_oben_throat, \
                            [dim_l_T,P4_throat,0.], \
                            [dim_l_T,dim_h_T,0.], \
                            [dim_l_T,dim_h_T,dim_h], \
                            [dim_l_T,P4_throat,P4] ) ) 
      
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 9 ############################       
    blocks.append(block(nr5,nr1,ni3))
            
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen, \
                            [dim_l,P4,P4],\
                            [dim_l,dim_t,dim_t], \
                            [dim_l,0.,dim_t], \
                            [dim_l,0.,P4]))  
             
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_throat, \
                            [dim_l_T,P4_throat,P4],\
                            [dim_l_T,dim_h_T,dim_t], \
                            [dim_l_T,0.,dim_t], \
                            [dim_l_T,0.,P4]))
           
    blocks[-1].make_grid()
    blocks[-1].rotate(1) 
#
#                        ENGSTER QUERSCHNITT FLACHER TEIL 
#
    
########################### 1 ############################      
    blocks.append(block(nr1,nr1,ni4))
             
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,0.,0.], \
                            [dim_l_T,P1_throat,0.], \
                            [dim_l_T,P1_throat,P1], \
                            [dim_l_T,0.,P1]) )   
                                 
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,0.,0.], \
                            [dim_l_T2,P1_throat,0.], \
                            [dim_l_T2,P1_throat,P1], \
                            [dim_l_T2,0.,P1]) )                             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 2 ############################      
    blocks.append(block(nr2,nr1,ni4))
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P1_throat,0.], \
                            [dim_l_T,P2_throat,0.], \
                            [dim_l_T,P2_throat,P2], \
                            [dim_l_T,P1_throat,P1] )) 
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P1_throat,0.], \
                            [dim_l_T2,P2_throat,0.], \
                            [dim_l_T2,P2_throat,P2], \
                            [dim_l_T2,P1_throat,P1] ))  
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 3 ############################       
    blocks.append(block(nr2,nr1,ni4))
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P1_throat,P1],\
                            [dim_l_T,P2_throat,P2], \
                            [dim_l_T,0.,P2], \
                            [dim_l_T,0.,P1] ))  
          
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P1_throat,P1],\
                            [dim_l_T2,P2_throat,P2], \
                            [dim_l_T2,0.,P2], \
                            [dim_l_T2,0.,P1] ))  
        
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 4 ############################  
    blocks.append(block(nr3,nr1,ni4))
         
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P2_throat,0.], \
                            [dim_l_T,P3_throat,0.], \
                            [dim_l_T,P3_throat,P3], \
                            [dim_l_T,P2_throat,P2] ) )  
         
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P2_throat,0.], \
                            [dim_l_T2,P3_throat,0.], \
                            [dim_l_T2,P3_throat,P3], \
                            [dim_l_T2,P2_throat,P2] ) )   
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 5 ############################       
    blocks.append(block(nr3,nr1,ni4))
          
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P2_throat,P2],\
                            [dim_l_T,P3_throat,P3], \
                            [dim_l_T,0.,P3], \
                            [dim_l_T,0.,P2] )) 
           
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P2_throat,P2],\
                            [dim_l_T2,P3_throat,P3], \
                            [dim_l_T2,0.,P3], \
                            [dim_l_T2,0.,P2] )) 
         
    blocks[-1].make_grid()
    blocks[-1].rotate(1)  
  
########################### 6 ############################        
    blocks.append(block(nr4,nr1,ni4))
         
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P3_throat,0.], \
                            [dim_l_T,P4_throat,0.], \
                            [dim_l_T,P4_throat,P4], \
                            [dim_l_T,P3_throat,P3] ) )  
         
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P3_throat,0.], \
                            [dim_l_T2,P4_throat,0.], \
                            [dim_l_T2,P4_throat,P4], \
                            [dim_l_T2,P3_throat,P3] ) ) 
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
         
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni4))
          
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T,P3_throat,P3],\
                            [dim_l_T,P4_throat,P4], \
                            [dim_l_T,0.,P4], \
                            [dim_l_T,0.,P3]))
           
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P3_throat,P3],\
                            [dim_l_T2,P4_throat,P4], \
                            [dim_l_T2,0.,P4], \
                            [dim_l_T2,0.,P3]))
         
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 8 ############################      
    blocks.append(block(nr5,nr1,ni4))
          
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_oben_throat, \
                            [dim_l_T,P4_throat,0.], \
                            [dim_l_T,dim_h_T,0.], \
                            [dim_l_T,dim_h_T,dim_h], \
                            [dim_l_T,P4_throat,P4] ) )  
          
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_oben_throat, \
                            [dim_l_T2,P4_throat,0.], \
                            [dim_l_T2,dim_h_T,0.], \
                            [dim_l_T2,dim_h_T,dim_h], \
                            [dim_l_T2,P4_throat,P4] ) ) 
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 9 ############################       
    blocks.append(block(nr5,nr1,ni4))
            
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_throat, \
                            [dim_l_T,P4_throat,P4],\
                            [dim_l_T,dim_h_T,dim_t], \
                            [dim_l_T,0.,dim_t], \
                            [dim_l_T,0.,P4]))
            
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_throat, \
                            [dim_l_T2,P4_throat,P4],\
                            [dim_l_T2,dim_h_T,dim_t], \
                            [dim_l_T2,0.,dim_t], \
                            [dim_l_T2,0.,P4]))
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1) 
   
#
#                         DIVERGIERENDER TEIL
#
   
   
########################### 1 ############################
    blocks.append(block(nr1,nr1,ni5))
            
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,0.,0.], \
                            [dim_l_T2,P1_throat,0.], \
                            [dim_l_T2,P1_throat,P1], \
                            [dim_l_T2,0.,P1]) ) 
                                
    blocks[-1].Ebenen.append(viereck(nr1,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,0.,0.], \
                            [dim_l_T3,P1_exit,0.], \
                            [dim_l_T3,P1_exit,P1], \
                            [dim_l_T3,0.,P1]) )                             
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
       
########################### 2 ############################
    blocks.append(block(nr2,nr1,ni5))
        
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P1_throat,0.], \
                            [dim_l_T2,P2_throat,0.], \
                            [dim_l_T2,P2_throat,P2], \
                            [dim_l_T2,P1_throat,P1] ))   
        
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P1_exit,0.], \
                            [dim_l_T3,P2_exit,0.], \
                            [dim_l_T3,P2_exit,P2], \
                            [dim_l_T3,P1_exit,P1] ))   
   
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 3 ############################      
    blocks.append(block(nr2,nr1,ni5))
        
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P1_throat,P1],\
                            [dim_l_T2,P2_throat,P2], \
                            [dim_l_T2,0.,P2], \
                            [dim_l_T2,0.,P1] ))  
         
    blocks[-1].Ebenen.append(viereck(nr2,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P1_exit,P1],\
                            [dim_l_T3,P2_exit,P2], \
                            [dim_l_T3,0.,P2], \
                            [dim_l_T3,0.,P1] ))  
       
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
   
########################### 4 ############################
    blocks.append(block(nr3,nr1,ni5))
         
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P2_throat,0.], \
                            [dim_l_T2,P3_throat,0.], \
                            [dim_l_T2,P3_throat,P3], \
                            [dim_l_T2,P2_throat,P2] ) )    
         
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P2_exit,0.], \
                            [dim_l_T3,P3_exit,0.], \
                            [dim_l_T3,P3_exit,P3], \
                            [dim_l_T3,P2_exit,P2] ) )  
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 5 ############################       
    blocks.append(block(nr3,nr1,ni5))
          
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P2_throat,P2],\
                            [dim_l_T2,P3_throat,P3], \
                            [dim_l_T2,0.,P3], \
                            [dim_l_T2,0.,P2] )) 
           
    blocks[-1].Ebenen.append(viereck(nr3,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P2_exit,P2],\
                            [dim_l_T3,P3_exit,P3], \
                            [dim_l_T3,0.,P3], \
                            [dim_l_T3,0.,P2] )) 
         
    blocks[-1].make_grid()
    blocks[-1].rotate(1)  
  
########################### 6 ############################        
    blocks.append(block(nr4,nr1,ni5))
         
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P3_throat,0.], \
                            [dim_l_T2,P4_throat,0.], \
                            [dim_l_T2,P4_throat,P4], \
                            [dim_l_T2,P3_throat,P3] ) )  
         
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P3_exit,0.], \
                            [dim_l_T3,P4_exit,0.], \
                            [dim_l_T3,P4_exit,P4], \
                            [dim_l_T3,P3_exit,P3] ) )   
    
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 7 ############################       
    blocks.append(block(nr4,nr1,ni5))
          
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T2,P3_throat,P3],\
                            [dim_l_T2,P4_throat,P4], \
                            [dim_l_T2,0.,P4], \
                            [dim_l_T2,0.,P3]))
           
    blocks[-1].Ebenen.append(viereck(nr4,nr1, \
                            RW_ideal_ogrid, \
                            [dim_l_T3,P3_exit,P3],\
                            [dim_l_T3,P4_exit,P4], \
                            [dim_l_T3,0.,P4], \
                            [dim_l_T3,0.,P3]))
         
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 8 ############################      
    blocks.append(block(nr5,nr1,ni5))
          
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_oben_throat, \
                            [dim_l_T2,P4_throat,0.], \
                            [dim_l_T2,dim_h_T,0.], \
                            [dim_l_T2,dim_h_T,dim_h], \
                            [dim_l_T2,P4_throat,P4] ) )  
          
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_oben_exit, \
                            [dim_l_T3,P4_exit,0.], \
                            [dim_l_T3,dim_h_e,0.], \
                            [dim_l_T3,dim_h_e,dim_h], \
                            [dim_l_T3,P4_exit,P4] ) ) 
     
    blocks[-1].make_grid()
    blocks[-1].rotate(1)
  
########################### 9 ############################       
    blocks.append(block(nr5,nr1,ni5))
            
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_throat, \
                            [dim_l_T2,P4_throat,P4],\
                            [dim_l_T2,dim_h_T,dim_t], \
                            [dim_l_T2,0.,dim_t], \
                            [dim_l_T2,0.,P4]))
            
    blocks[-1].Ebenen.append(viereck(nr5,nr1, \
                            RW_ideal_ogrid_aussen_exit, \
                            [dim_l_T3,P4_exit,P4],\
                            [dim_l_T3,dim_h_e,dim_t], \
                            [dim_l_T3,0.,dim_t], \
                            [dim_l_T3,0.,P4]))
          
    blocks[-1].make_grid()
    blocks[-1].rotate(1)    

    return blocks
