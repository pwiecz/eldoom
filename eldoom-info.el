;;; eldoom-info.el --- mobjinfo table per thing type -*- lexical-binding:t -*- 

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Per-type radius, height, speed, damage, health, and flags modelled on
;; vanilla mobjinfo_t / Dehacked.  Values are map units unless noted as
;; fixed_t (radius-fx, height-fx, speed-fx).

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-math)

(defconst enf-PST_LIVE 0)
(defconst enf-PST_DEAD 1)
(defconst enf-PST_REBORN 2)

(defconst enf-MF_SPECIAL           1)
(defconst enf-MF_SOLID             2)
(defconst enf-MF_SHOOTABLE         4)
(defconst enf-MF_NOSECTOR          8)
(defconst enf-MF_NOBLOCKMAP       16)
(defconst enf-MF_AMBUSH           32)
(defconst enf-MF_JUSTHIT          64)
(defconst enf-MF_JUSTATTACKED    128)
(defconst enf-MF_SPAWNCEILING    256)
(defconst enf-MF_NOGRAVITY       512)
(defconst enf-MF_DROPOFF       #x400)
(defconst enf-MF_PICKUP        #x800)
(defconst enf-MF_NOCLIP       #x1000)
(defconst enf-MF_SLIDE        #x2000)
(defconst enf-MF_FLOAT        #x4000)
(defconst enf-MF_TELEPORT     #x8000)
(defconst enf-MF_MISSILE     #x10000)
(defconst enf-MF_DROPPED     #x20000)
(defconst enf-MF_SHADOW      #x40000)
(defconst enf-MF_NOBLOOD     #x80000)
(defconst enf-MF_CORPSE     #x100000)
(defconst enf-MF_INFLOAT    #x200000)
(defconst enf-MF_COUNTKILL  #x400000)
(defconst enf-MF_COUNTITEM  #x800000)
(defconst enf-MF_SKULLFLY  #x1000000)
(defconst enf-MF_NOTDMATCH #x2000000)

(defconst enf-ML_BLOCKING       1)
(defconst enf-ML_BLOCKMONSTERS  2)
(defconst enf-ML_TWOSIDED       4)
(defconst enf-ML_DONTPEGTOP     8)
(defconst enf-ML_DONTPEGBOTTOM 16)
(defconst enf-ML_SECRET        32)
(defconst enf-ML_SOUNDBLOCK    64)
(defconst enf-ML_DONTDRAW     128)
(defconst enf-ML_MAPPED       256)

(defconst enf-MTF_EASY 1)
(defconst enf-MTF_NORMAL 2)
(defconst enf-MTF_HARD 4)
(defconst enf-MTF_AMBUSH 8)

(defconst enf-sprite-names ["TROO" "SHTG" "PUNG" "PISG" "PISF" "SHTF" "SHT2" "CHGG" "CHGF" "MISG" 
                            "MISF" "SAWG" "PLSG" "PLSF" "BFGG" "BFGF" "BLUD" "PUFF" "BAL1" "BAL2" 
                            "PLSS" "PLSE" "MISL" "BFS1" "BFE1" "BFE2" "TFOG" "IFOG" "PLAY" "POSS" 
                            "SPOS" "VILE" "FIRE" "FATB" "FBXP" "SKEL" "MANF" "FATT" "CPOS" "SARG" 
                            "HEAD" "BAL7" "BOSS" "BOS2" "SKUL" "SPID" "BSPI" "APLS" "APBX" "CYBR" 
                            "PAIN" "SSWV" "KEEN" "BBRN" "BOSF" "ARM1" "ARM2" "BAR1" "BEXP" "FCAN" 
                            "BON1" "BON2" "BKEY" "RKEY" "YKEY" "BSKU" "RSKU" "YSKU" "STIM" "MEDI" 
                            "SOUL" "PINV" "PSTR" "PINS" "MEGA" "SUIT" "PMAP" "PVIS" "CLIP" "AMMO" 
                            "ROCK" "BROK" "CELL" "CELP" "SHEL" "SBOX" "BPAK" "BFUG" "MGUN" "CSAW" 
                            "LAUN" "PLAS" "SHOT" "SGN2" "COLU" "SMT2" "GOR1" "POL2" "POL5" "POL4" 
                            "POL3" "POL1" "POL6" "GOR2" "GOR3" "GOR4" "GOR5" "SMIT" "COL1" "COL2" 
                            "COL3" "COL4" "CAND" "CBRA" "COL6" "TRE1" "TRE2" "ELEC" "CEYE" "FSKU" 
                            "COL5" "TBLU" "TGRN" "TRED" "SMBT" "SMGT" "SMRT" "HDB1" "HDB2" "HDB3" 
                            "HDB4" "HDB5" "HDB6" "POB1" "POB2" "BRS1" "TLMP" "TLP2"])

(let ((c 0))
  (cl-loop for sprite-name across enf-sprite-names do
           (let ((name (intern (format "eldoom-info-SPR_%s" sprite-name))))
             (eval `(defconst ,name ,c)))
           (cl-incf c)))

(let ((c 0))
  (dolist (n '(S_NULL 
               S_LIGHTDONE S_PUNCH S_PUNCHDOWN S_PUNCHUP S_PUNCH1 S_PUNCH2
               S_PUNCH3 S_PUNCH4 S_PUNCH5 S_PISTOL S_PISTOLDOWN S_PISTOLUP
               S_PISTOL1 S_PISTOL2 S_PISTOL3 S_PISTOL4 S_PISTOLFLASH S_SGUN
               S_SGUNDOWN S_SGUNUP S_SGUN1 S_SGUN2 S_SGUN3 S_SGUN4 S_SGUN5
               S_SGUN6 S_SGUN7 S_SGUN8 S_SGUN9 S_SGUNFLASH1 S_SGUNFLASH2 S_DSGUN
               S_DSGUNDOWN S_DSGUNUP S_DSGUN1 S_DSGUN2 S_DSGUN3 S_DSGUN4
               S_DSGUN5 S_DSGUN6 S_DSGUN7 S_DSGUN8 S_DSGUN9 S_DSGUN10 S_DSNR1
               S_DSNR2 S_DSGUNFLASH1 S_DSGUNFLASH2 S_CHAIN S_CHAINDOWN S_CHAINUP
               S_CHAIN1 S_CHAIN2 S_CHAIN3 S_CHAINFLASH1 S_CHAINFLASH2 S_MISSILE
               S_MISSILEDOWN S_MISSILEUP S_MISSILE1 S_MISSILE2 S_MISSILE3
               S_MISSILEFLASH1 S_MISSILEFLASH2 S_MISSILEFLASH3 S_MISSILEFLASH4
               S_SAW S_SAWB S_SAWDOWN S_SAWUP S_SAW1 S_SAW2 S_SAW3 S_PLASMA
               S_PLASMADOWN S_PLASMAUP S_PLASMA1 S_PLASMA2 S_PLASMAFLASH1
               S_PLASMAFLASH2 S_BFG S_BFGDOWN S_BFGUP S_BFG1 S_BFG2 S_BFG3
               S_BFG4 S_BFGFLASH1 S_BFGFLASH2 S_BLOOD1 S_BLOOD2 S_BLOOD3 S_PUFF1
               S_PUFF2 S_PUFF3 S_PUFF4 S_TBALL1 S_TBALL2 S_TBALLX1 S_TBALLX2 
               S_TBALLX3 S_RBALL1 S_RBALL2 S_RBALLX1 S_RBALLX2 S_RBALLX3
               S_PLASBALL S_PLASBALL2 S_PLASEXP S_PLASEXP2 S_PLASEXP3 S_PLASEXP4
               S_PLASEXP5 S_ROCKET S_BFGSHOT S_BFGSHOT2 S_BFGLAND S_BFGLAND2
               S_BFGLAND3 S_BFGLAND4 S_BFGLAND5 S_BFGLAND6 S_BFGEXP S_BFGEXP2
               S_BFGEXP3 S_BFGEXP4 S_EXPLODE1 S_EXPLODE2 S_EXPLODE3 S_TFOG
               S_TFOG01 S_TFOG02 S_TFOG2 S_TFOG3 S_TFOG4 S_TFOG5 S_TFOG6 S_TFOG7
               S_TFOG8 S_TFOG9 S_TFOG10 S_IFOG S_IFOG01 S_IFOG02 S_IFOG2 S_IFOG3
               S_IFOG4 S_IFOG5 S_PLAY S_PLAY_RUN1 S_PLAY_RUN2 S_PLAY_RUN3
               S_PLAY_RUN4 S_PLAY_ATK1 S_PLAY_ATK2 S_PLAY_PAIN S_PLAY_PAIN2
               S_PLAY_DIE1 S_PLAY_DIE2 S_PLAY_DIE3 S_PLAY_DIE4 S_PLAY_DIE5
               S_PLAY_DIE6 S_PLAY_DIE7 S_PLAY_XDIE1 S_PLAY_XDIE2 S_PLAY_XDIE3
               S_PLAY_XDIE4 S_PLAY_XDIE5 S_PLAY_XDIE6 S_PLAY_XDIE7 S_PLAY_XDIE8
               S_PLAY_XDIE9 S_POSS_STND S_POSS_STND2 S_POSS_RUN1 S_POSS_RUN2
               S_POSS_RUN3 S_POSS_RUN4 S_POSS_RUN5 S_POSS_RUN6 S_POSS_RUN7
               S_POSS_RUN8 S_POSS_ATK1 S_POSS_ATK2 S_POSS_ATK3 S_POSS_PAIN
               S_POSS_PAIN2 S_POSS_DIE1 S_POSS_DIE2 S_POSS_DIE3 S_POSS_DIE4
               S_POSS_DIE5 S_POSS_XDIE1 S_POSS_XDIE2 S_POSS_XDIE3 S_POSS_XDIE4
               S_POSS_XDIE5 S_POSS_XDIE6 S_POSS_XDIE7 S_POSS_XDIE8 S_POSS_XDIE9
               S_POSS_RAISE1 S_POSS_RAISE2 S_POSS_RAISE3 S_POSS_RAISE4
               S_SPOS_STND S_SPOS_STND2 S_SPOS_RUN1 S_SPOS_RUN2 S_SPOS_RUN3
               S_SPOS_RUN4 S_SPOS_RUN5 S_SPOS_RUN6 S_SPOS_RUN7 S_SPOS_RUN8
               S_SPOS_ATK1 S_SPOS_ATK2 S_SPOS_ATK3 S_SPOS_PAIN S_SPOS_PAIN2
               S_SPOS_DIE1 S_SPOS_DIE2 S_SPOS_DIE3 S_SPOS_DIE4 S_SPOS_DIE5
               S_SPOS_XDIE1 S_SPOS_XDIE2 S_SPOS_XDIE3 S_SPOS_XDIE4 S_SPOS_XDIE5
               S_SPOS_XDIE6 S_SPOS_XDIE7 S_SPOS_XDIE8 S_SPOS_XDIE9 S_SPOS_RAISE1
               S_SPOS_RAISE2 S_SPOS_RAISE3 S_SPOS_RAISE4 S_SPOS_RAISE5
               S_VILE_STND S_VILE_STND2 S_VILE_RUN1 S_VILE_RUN2 S_VILE_RUN3
               S_VILE_RUN4 S_VILE_RUN5 S_VILE_RUN6 S_VILE_RUN7 S_VILE_RUN8
               S_VILE_RUN9 S_VILE_RUN10 S_VILE_RUN11 S_VILE_RUN12 S_VILE_ATK1
               S_VILE_ATK2 S_VILE_ATK3 S_VILE_ATK4 S_VILE_ATK5 S_VILE_ATK6
               S_VILE_ATK7 S_VILE_ATK8 S_VILE_ATK9 S_VILE_ATK10 S_VILE_ATK11
               S_VILE_HEAL1 S_VILE_HEAL2 S_VILE_HEAL3 S_VILE_PAIN S_VILE_PAIN2
               S_VILE_DIE1 S_VILE_DIE2 S_VILE_DIE3 S_VILE_DIE4 S_VILE_DIE5
               S_VILE_DIE6 S_VILE_DIE7 S_VILE_DIE8 S_VILE_DIE9 S_VILE_DIE10
               S_FIRE1 S_FIRE2 S_FIRE3 S_FIRE4 S_FIRE5 S_FIRE6 S_FIRE7 S_FIRE8
               S_FIRE9 S_FIRE10 S_FIRE11 S_FIRE12 S_FIRE13 S_FIRE14 S_FIRE15
               S_FIRE16 S_FIRE17 S_FIRE18 S_FIRE19 S_FIRE20 S_FIRE21 S_FIRE22
               S_FIRE23 S_FIRE24 S_FIRE25 S_FIRE26 S_FIRE27 S_FIRE28 S_FIRE29
               S_FIRE30 S_SMOKE1 S_SMOKE2 S_SMOKE3 S_SMOKE4 S_SMOKE5 S_TRACER
               S_TRACER2 S_TRACEEXP1 S_TRACEEXP2 S_TRACEEXP3 S_SKEL_STND
               S_SKEL_STND2 S_SKEL_RUN1 S_SKEL_RUN2 S_SKEL_RUN3 S_SKEL_RUN4
               S_SKEL_RUN5 S_SKEL_RUN6 S_SKEL_RUN7 S_SKEL_RUN8 S_SKEL_RUN9
               S_SKEL_RUN10 S_SKEL_RUN11 S_SKEL_RUN12 S_SKEL_FIST1 S_SKEL_FIST2
               S_SKEL_FIST3 S_SKEL_FIST4 S_SKEL_MISS1 S_SKEL_MISS2 S_SKEL_MISS3
               S_SKEL_MISS4 S_SKEL_PAIN S_SKEL_PAIN2 S_SKEL_DIE1 S_SKEL_DIE2
               S_SKEL_DIE3 S_SKEL_DIE4 S_SKEL_DIE5 S_SKEL_DIE6 S_SKEL_RAISE1
               S_SKEL_RAISE2 S_SKEL_RAISE3 S_SKEL_RAISE4 S_SKEL_RAISE5
               S_SKEL_RAISE6 S_FATSHOT1 S_FATSHOT2 S_FATSHOTX1 S_FATSHOTX2
               S_FATSHOTX3 S_FATT_STND S_FATT_STND2 S_FATT_RUN1 S_FATT_RUN2
               S_FATT_RUN3 S_FATT_RUN4 S_FATT_RUN5 S_FATT_RUN6 S_FATT_RUN7
               S_FATT_RUN8 S_FATT_RUN9 S_FATT_RUN10 S_FATT_RUN11 S_FATT_RUN12
               S_FATT_ATK1 S_FATT_ATK2 S_FATT_ATK3 S_FATT_ATK4 S_FATT_ATK5
               S_FATT_ATK6 S_FATT_ATK7 S_FATT_ATK8 S_FATT_ATK9 S_FATT_ATK10
               S_FATT_PAIN S_FATT_PAIN2 S_FATT_DIE1 S_FATT_DIE2 S_FATT_DIE3
               S_FATT_DIE4 S_FATT_DIE5 S_FATT_DIE6 S_FATT_DIE7 S_FATT_DIE8
               S_FATT_DIE9 S_FATT_DIE10 S_FATT_RAISE1 S_FATT_RAISE2
               S_FATT_RAISE3 S_FATT_RAISE4 S_FATT_RAISE5 S_FATT_RAISE6
               S_FATT_RAISE7 S_FATT_RAISE8 S_CPOS_STND S_CPOS_STND2 S_CPOS_RUN1
               S_CPOS_RUN2 S_CPOS_RUN3 S_CPOS_RUN4 S_CPOS_RUN5 S_CPOS_RUN6
               S_CPOS_RUN7 S_CPOS_RUN8 S_CPOS_ATK1 S_CPOS_ATK2 S_CPOS_ATK3
               S_CPOS_ATK4 S_CPOS_PAIN S_CPOS_PAIN2 S_CPOS_DIE1 S_CPOS_DIE2
               S_CPOS_DIE3 S_CPOS_DIE4 S_CPOS_DIE5 S_CPOS_DIE6 S_CPOS_DIE7
               S_CPOS_XDIE1 S_CPOS_XDIE2 S_CPOS_XDIE3 S_CPOS_XDIE4 S_CPOS_XDIE5
               S_CPOS_XDIE6 S_CPOS_RAISE1 S_CPOS_RAISE2 S_CPOS_RAISE3
               S_CPOS_RAISE4 S_CPOS_RAISE5 S_CPOS_RAISE6 S_CPOS_RAISE7
               S_TROO_STND S_TROO_STND2 S_TROO_RUN1 S_TROO_RUN2 S_TROO_RUN3
               S_TROO_RUN4 S_TROO_RUN5 S_TROO_RUN6 S_TROO_RUN7 S_TROO_RUN8
               S_TROO_ATK1 S_TROO_ATK2 S_TROO_ATK3 S_TROO_PAIN S_TROO_PAIN2
               S_TROO_DIE1 S_TROO_DIE2 S_TROO_DIE3 S_TROO_DIE4 S_TROO_DIE5
               S_TROO_XDIE1 S_TROO_XDIE2 S_TROO_XDIE3 S_TROO_XDIE4 S_TROO_XDIE5
               S_TROO_XDIE6 S_TROO_XDIE7 S_TROO_XDIE8 S_TROO_RAISE1
               S_TROO_RAISE2 S_TROO_RAISE3 S_TROO_RAISE4 S_TROO_RAISE5
               S_SARG_STND S_SARG_STND2 S_SARG_RUN1 S_SARG_RUN2 S_SARG_RUN3
               S_SARG_RUN4 S_SARG_RUN5 S_SARG_RUN6 S_SARG_RUN7 S_SARG_RUN8
               S_SARG_ATK1 S_SARG_ATK2 S_SARG_ATK3 S_SARG_PAIN S_SARG_PAIN2
               S_SARG_DIE1 S_SARG_DIE2 S_SARG_DIE3 S_SARG_DIE4 S_SARG_DIE5
               S_SARG_DIE6 S_SARG_RAISE1 S_SARG_RAISE2 S_SARG_RAISE3
               S_SARG_RAISE4 S_SARG_RAISE5 S_SARG_RAISE6 S_HEAD_STND S_HEAD_RUN1
               S_HEAD_ATK1 S_HEAD_ATK2 S_HEAD_ATK3 S_HEAD_PAIN S_HEAD_PAIN2
               S_HEAD_PAIN3 S_HEAD_DIE1 S_HEAD_DIE2 S_HEAD_DIE3 S_HEAD_DIE4
               S_HEAD_DIE5 S_HEAD_DIE6 S_HEAD_RAISE1 S_HEAD_RAISE2 S_HEAD_RAISE3
               S_HEAD_RAISE4 S_HEAD_RAISE5 S_HEAD_RAISE6 S_BRBALL1 S_BRBALL2
               S_BRBALLX1 S_BRBALLX2 S_BRBALLX3 S_BOSS_STND S_BOSS_STND2
               S_BOSS_RUN1 S_BOSS_RUN2 S_BOSS_RUN3 S_BOSS_RUN4 S_BOSS_RUN5
               S_BOSS_RUN6 S_BOSS_RUN7 S_BOSS_RUN8 S_BOSS_ATK1 S_BOSS_ATK2
               S_BOSS_ATK3 S_BOSS_PAIN S_BOSS_PAIN2 S_BOSS_DIE1 S_BOSS_DIE2
               S_BOSS_DIE3 S_BOSS_DIE4 S_BOSS_DIE5 S_BOSS_DIE6 S_BOSS_DIE7
               S_BOSS_RAISE1 S_BOSS_RAISE2 S_BOSS_RAISE3 S_BOSS_RAISE4
               S_BOSS_RAISE5 S_BOSS_RAISE6 S_BOSS_RAISE7 S_BOS2_STND
               S_BOS2_STND2 S_BOS2_RUN1 S_BOS2_RUN2 S_BOS2_RUN3 S_BOS2_RUN4
               S_BOS2_RUN5 S_BOS2_RUN6 S_BOS2_RUN7 S_BOS2_RUN8 S_BOS2_ATK1
               S_BOS2_ATK2 S_BOS2_ATK3 S_BOS2_PAIN S_BOS2_PAIN2 S_BOS2_DIE1
               S_BOS2_DIE2 S_BOS2_DIE3 S_BOS2_DIE4 S_BOS2_DIE5 S_BOS2_DIE6
               S_BOS2_DIE7 S_BOS2_RAISE1 S_BOS2_RAISE2 S_BOS2_RAISE3
               S_BOS2_RAISE4 S_BOS2_RAISE5 S_BOS2_RAISE6 S_BOS2_RAISE7
               S_SKULL_STND S_SKULL_STND2 S_SKULL_RUN1 S_SKULL_RUN2 S_SKULL_ATK1
               S_SKULL_ATK2 S_SKULL_ATK3 S_SKULL_ATK4 S_SKULL_PAIN S_SKULL_PAIN2
               S_SKULL_DIE1 S_SKULL_DIE2 S_SKULL_DIE3 S_SKULL_DIE4 S_SKULL_DIE5
               S_SKULL_DIE6 S_SPID_STND S_SPID_STND2 S_SPID_RUN1 S_SPID_RUN2
               S_SPID_RUN3 S_SPID_RUN4 S_SPID_RUN5 S_SPID_RUN6 S_SPID_RUN7
               S_SPID_RUN8 S_SPID_RUN9 S_SPID_RUN10 S_SPID_RUN11 S_SPID_RUN12
               S_SPID_ATK1 S_SPID_ATK2 S_SPID_ATK3 S_SPID_ATK4 S_SPID_PAIN
               S_SPID_PAIN2 S_SPID_DIE1 S_SPID_DIE2 S_SPID_DIE3 S_SPID_DIE4
               S_SPID_DIE5 S_SPID_DIE6 S_SPID_DIE7 S_SPID_DIE8 S_SPID_DIE9
               S_SPID_DIE10 S_SPID_DIE11 S_BSPI_STND S_BSPI_STND2 S_BSPI_SIGHT
               S_BSPI_RUN1 S_BSPI_RUN2 S_BSPI_RUN3 S_BSPI_RUN4 S_BSPI_RUN5
               S_BSPI_RUN6 S_BSPI_RUN7 S_BSPI_RUN8 S_BSPI_RUN9 S_BSPI_RUN10
               S_BSPI_RUN11 S_BSPI_RUN12 S_BSPI_ATK1 S_BSPI_ATK2 S_BSPI_ATK3
               S_BSPI_ATK4 S_BSPI_PAIN S_BSPI_PAIN2 S_BSPI_DIE1 S_BSPI_DIE2
               S_BSPI_DIE3 S_BSPI_DIE4 S_BSPI_DIE5 S_BSPI_DIE6 S_BSPI_DIE7
               S_BSPI_RAISE1 S_BSPI_RAISE2 S_BSPI_RAISE3 S_BSPI_RAISE4
               S_BSPI_RAISE5 S_BSPI_RAISE6 S_BSPI_RAISE7 S_ARACH_PLAZ
               S_ARACH_PLAZ2 S_ARACH_PLEX S_ARACH_PLEX2 S_ARACH_PLEX3
               S_ARACH_PLEX4 S_ARACH_PLEX5 S_CYBER_STND S_CYBER_STND2
               S_CYBER_RUN1 S_CYBER_RUN2 S_CYBER_RUN3 S_CYBER_RUN4 S_CYBER_RUN5
               S_CYBER_RUN6 S_CYBER_RUN7 S_CYBER_RUN8 S_CYBER_ATK1 S_CYBER_ATK2
               S_CYBER_ATK3 S_CYBER_ATK4 S_CYBER_ATK5 S_CYBER_ATK6 S_CYBER_PAIN
               S_CYBER_DIE1 S_CYBER_DIE2 S_CYBER_DIE3 S_CYBER_DIE4 S_CYBER_DIE5
               S_CYBER_DIE6 S_CYBER_DIE7 S_CYBER_DIE8 S_CYBER_DIE9 S_CYBER_DIE10
               S_PAIN_STND S_PAIN_RUN1 S_PAIN_RUN2 S_PAIN_RUN3 S_PAIN_RUN4
               S_PAIN_RUN5 S_PAIN_RUN6 S_PAIN_ATK1 S_PAIN_ATK2 S_PAIN_ATK3
               S_PAIN_ATK4 S_PAIN_PAIN S_PAIN_PAIN2 S_PAIN_DIE1 S_PAIN_DIE2
               S_PAIN_DIE3 S_PAIN_DIE4 S_PAIN_DIE5 S_PAIN_DIE6 S_PAIN_RAISE1
               S_PAIN_RAISE2 S_PAIN_RAISE3 S_PAIN_RAISE4 S_PAIN_RAISE5
               S_PAIN_RAISE6 S_SSWV_STND S_SSWV_STND2 S_SSWV_RUN1 S_SSWV_RUN2
               S_SSWV_RUN3 S_SSWV_RUN4 S_SSWV_RUN5 S_SSWV_RUN6 S_SSWV_RUN7
               S_SSWV_RUN8 S_SSWV_ATK1 S_SSWV_ATK2 S_SSWV_ATK3 S_SSWV_ATK4
               S_SSWV_ATK5 S_SSWV_ATK6 S_SSWV_PAIN S_SSWV_PAIN2 S_SSWV_DIE1
               S_SSWV_DIE2 S_SSWV_DIE3 S_SSWV_DIE4 S_SSWV_DIE5 S_SSWV_XDIE1
               S_SSWV_XDIE2 S_SSWV_XDIE3 S_SSWV_XDIE4 S_SSWV_XDIE5 S_SSWV_XDIE6
               S_SSWV_XDIE7 S_SSWV_XDIE8 S_SSWV_XDIE9 S_SSWV_RAISE1 
               S_SSWV_RAISE2 S_SSWV_RAISE3 S_SSWV_RAISE4 S_SSWV_RAISE5
               S_KEENSTND S_COMMKEEN S_COMMKEEN2 S_COMMKEEN3 S_COMMKEEN4
               S_COMMKEEN5 S_COMMKEEN6 S_COMMKEEN7 S_COMMKEEN8 S_COMMKEEN9
               S_COMMKEEN10 S_COMMKEEN11 S_COMMKEEN12 S_KEENPAIN S_KEENPAIN2
               S_BRAIN S_BRAIN_PAIN S_BRAIN_DIE1 S_BRAIN_DIE2 S_BRAIN_DIE3
               S_BRAIN_DIE4 S_BRAINEYE S_BRAINEYESEE S_BRAINEYE1 S_SPAWN1
               S_SPAWN2 S_SPAWN3 S_SPAWN4 S_SPAWNFIRE1 S_SPAWNFIRE2 S_SPAWNFIRE3
               S_SPAWNFIRE4 S_SPAWNFIRE5 S_SPAWNFIRE6 S_SPAWNFIRE7 S_SPAWNFIRE8
               S_BRAINEXPLODE1 S_BRAINEXPLODE2 S_BRAINEXPLODE3 S_ARM1 S_ARM1A
               S_ARM2 S_ARM2A S_BAR1 S_BAR2 S_BEXP S_BEXP2 S_BEXP3 S_BEXP4
               S_BEXP5 S_BBAR1 S_BBAR2 S_BBAR3 S_BON1 S_BON1A S_BON1B S_BON1C
               S_BON1D S_BON1E S_BON2 S_BON2A S_BON2B S_BON2C S_BON2D S_BON2E
               S_BKEY S_BKEY2 S_RKEY S_RKEY2 S_YKEY S_YKEY2 S_BSKULL S_BSKULL2
               S_RSKULL S_RSKULL2 S_YSKULL S_YSKULL2 S_STIM S_MEDI S_SOUL
               S_SOUL2 S_SOUL3 S_SOUL4 S_SOUL5 S_SOUL6 S_PINV S_PINV2 S_PINV3
               S_PINV4 S_PSTR S_PINS S_PINS2 S_PINS3 S_PINS4 S_MEGA S_MEGA2
               S_MEGA3 S_MEGA4 S_SUIT S_PMAP S_PMAP2 S_PMAP3 S_PMAP4 S_PMAP5
               S_PMAP6 S_PVIS S_PVIS2 S_CLIP S_AMMO S_ROCK S_BROK S_CELL S_CELP
               S_SHEL S_SBOX S_BPAK S_BFUG S_MGUN S_CSAW S_LAUN S_PLAS S_SHOT
               S_SHOT2 S_COLU S_STALAG S_BLOODYTWITCH S_BLOODYTWITCH2
               S_BLOODYTWITCH3 S_BLOODYTWITCH4 S_DEADTORSO S_DEADBOTTOM
               S_HEADSONSTICK S_GIBS S_HEADONASTICK S_HEADCANDLES S_HEADCANDLES2
               S_DEADSTICK S_LIVESTICK S_LIVESTICK2 S_MEAT2 S_MEAT3 S_MEAT4
               S_MEAT5 S_STALAGTITE S_TALLGRNCOL S_SHRTGRNCOL S_TALLREDCOL 
               S_SHRTREDCOL S_CANDLESTIK S_CANDELABRA S_SKULLCOL S_TORCHTREE
               S_BIGTREE S_TECHPILLAR S_EVILEYE S_EVILEYE2 S_EVILEYE3 S_EVILEYE4
               S_FLOATSKULL S_FLOATSKULL2 S_FLOATSKULL3 S_HEARTCOL S_HEARTCOL2
               S_BLUETORCH S_BLUETORCH2 S_BLUETORCH3 S_BLUETORCH4 S_GREENTORCH
               S_GREENTORCH2 S_GREENTORCH3 S_GREENTORCH4 S_REDTORCH S_REDTORCH2
               S_REDTORCH3 S_REDTORCH4 S_BTORCHSHRT S_BTORCHSHRT2 S_BTORCHSHRT3
               S_BTORCHSHRT4 S_GTORCHSHRT S_GTORCHSHRT2 S_GTORCHSHRT3
               S_GTORCHSHRT4 S_RTORCHSHRT S_RTORCHSHRT2 S_RTORCHSHRT3
               S_RTORCHSHRT4 S_HANGNOGUTS S_HANGBNOBRAIN S_HANGTLOOKDN
               S_HANGTSKULL S_HANGTLOOKUP S_HANGTNOBRAIN S_COLONGIBS S_SMALLPOOL
               S_BRAINSTEM S_TECHLAMP S_TECHLAMP2 S_TECHLAMP3 S_TECHLAMP4
               S_TECH2LAMP S_TECH2LAMP2 S_TECH2LAMP3 S_TECH2LAMP4 NUMSTATES))
    (let ((name (intern (format "eldoom-info-%s" n))))
      (eval `(defconst ,name ,c)))
    (cl-incf c)))

(let ((c 0))
  (dolist (n '(MT_PLAYER
               MT_POSSESSED MT_SHOTGUY MT_VILE MT_FIRE MT_UNDEAD MT_TRACER 
               MT_SMOKE MT_FATSO MT_FATSHOT MT_CHAINGUY MT_TROOP MT_SERGEANT
               MT_SHADOWS MT_HEAD MT_BRUISER MT_BRUISERSHOT MT_KNIGHT MT_SKULL
               MT_SPIDER MT_BABY MT_CYBORG MT_PAIN MT_WOLFSS MT_KEEN
               MT_BOSSBRAIN MT_BOSSSPIT MT_BOSSTARGET MT_SPAWNSHOT MT_SPAWNFIRE
               MT_BARREL MT_TROOPSHOT MT_HEADSHOT MT_ROCKET MT_PLASMA MT_BFG
               MT_ARACHPLAZ MT_PUFF MT_BLOOD MT_TFOG MT_IFOG MT_TELEPORTMAN
               MT_EXTRABFG MT_MISC0 MT_MISC1 MT_MISC2 MT_MISC3 MT_MISC4 MT_MISC5
               MT_MISC6 MT_MISC7 MT_MISC8 MT_MISC9 MT_MISC10 MT_MISC11 MT_MISC12
               MT_INV MT_MISC13 MT_INS MT_MISC14 MT_MISC15 MT_MISC16 MT_MEGA
               MT_CLIP MT_MISC17 MT_MISC18 MT_MISC19 MT_MISC20 MT_MISC21 
               MT_MISC22 MT_MISC23 MT_MISC24 MT_MISC25 MT_CHAINGUN MT_MISC26
               MT_MISC27 MT_MISC28 MT_SHOTGUN MT_SUPERSHOTGUN MT_MISC29 
               MT_MISC30 MT_MISC31 MT_MISC32 MT_MISC33 MT_MISC34 MT_MISC35
               MT_MISC36 MT_MISC37 MT_MISC38 MT_MISC39 MT_MISC40 MT_MISC41
               MT_MISC42 MT_MISC43 MT_MISC44 MT_MISC45 MT_MISC46 MT_MISC47
               MT_MISC48 MT_MISC49 MT_MISC50 MT_MISC51 MT_MISC52 MT_MISC53
               MT_MISC54 MT_MISC55 MT_MISC56 MT_MISC57 MT_MISC58 MT_MISC59
               MT_MISC60 MT_MISC61 MT_MISC62 MT_MISC63 MT_MISC64 MT_MISC65
               MT_MISC66 MT_MISC67 MT_MISC68 MT_MISC69 MT_MISC70 MT_MISC71
               MT_MISC72 MT_MISC73 MT_MISC74 MT_MISC75 MT_MISC76 MT_MISC77
               MT_MISC78 MT_MISC79 MT_MISC80 MT_MISC81 MT_MISC82 MT_MISC83
               MT_MISC84 MT_MISC85 MT_MISC86 NUMMOBJTYPES))
    (let ((name (intern (format "eldoom-info-%s" n))))
      (eval `(defconst ,name ,c)))
    (cl-incf c)))

(cl-defstruct (enf-state (:constructor enf--make-state) (:copier nil))
  index
  name
  (sprite-index 0 :type fixnum)
  sprite
  (frame 0 :type fixnum)
  (fullbright nil :type boolean)
  (tics 0 :type fixnum)
  action-name
  action
  next)

(defconst enf--frame-mask #x7fff)
(defconst enf--FF_FULLBRIGHT #x8000)

(defsubst enf--def-state (states index name sprite-index frame tics action-name next)
  (aset states index (enf--make-state :index index :name name
                                      :sprite-index sprite-index 
                                      :frame (logand frame enf--frame-mask)
                                      :fullbright (/= 0 (logand frame enf--FF_FULLBRIGHT))
                                      :tics tics :action-name action-name :next next)))

(defsubst enf--state-or-nil (states index)
  (when (/= index enf-S_NULL)
    (aref states index)))

(defun enf--make-states (sprites)
  (let ((states (make-vector enf-NUMSTATES nil)))
    (enf--def-state states enf-S_NULL "S_NULL" enf-SPR_TROO 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_LIGHTDONE "S_LIGHTDONE" enf-SPR_SHTG 4 0 "A_Light0" enf-S_NULL)
    (enf--def-state states enf-S_PUNCH "S_PUNCH" enf-SPR_PUNG 0 1 "A_WeaponReady" enf-S_PUNCH)
    (enf--def-state states enf-S_PUNCHDOWN "S_PUNCHDOWN" enf-SPR_PUNG 0 1 "A_Lower" enf-S_PUNCHDOWN)
    (enf--def-state states enf-S_PUNCHUP "S_PUNCHUP" enf-SPR_PUNG 0 1 "A_Raise" enf-S_PUNCHUP)
    (enf--def-state states enf-S_PUNCH1 "S_PUNCH1" enf-SPR_PUNG 1 4 "NULL" enf-S_PUNCH2)
    (enf--def-state states enf-S_PUNCH2 "S_PUNCH2" enf-SPR_PUNG 2 4 "A_Punch" enf-S_PUNCH3)
    (enf--def-state states enf-S_PUNCH3 "S_PUNCH3" enf-SPR_PUNG 3 5 "NULL" enf-S_PUNCH4)
    (enf--def-state states enf-S_PUNCH4 "S_PUNCH4" enf-SPR_PUNG 2 4 "NULL" enf-S_PUNCH5)
    (enf--def-state states enf-S_PUNCH5 "S_PUNCH5" enf-SPR_PUNG 1 5 "A_ReFire" enf-S_PUNCH)
    (enf--def-state states enf-S_PISTOL "S_PISTOL" enf-SPR_PISG 0 1 "A_WeaponReady" enf-S_PISTOL)
    (enf--def-state states enf-S_PISTOLDOWN "S_PISTOLDOWN" enf-SPR_PISG 0 1 "A_Lower" enf-S_PISTOLDOWN)
    (enf--def-state states enf-S_PISTOLUP "S_PISTOLUP" enf-SPR_PISG 0 1 "A_Raise" enf-S_PISTOLUP)
    (enf--def-state states enf-S_PISTOL1 "S_PISTOL1" enf-SPR_PISG 0 4 "NULL" enf-S_PISTOL2)
    (enf--def-state states enf-S_PISTOL2 "S_PISTOL2" enf-SPR_PISG 1 6 "A_FirePistol" enf-S_PISTOL3)
    (enf--def-state states enf-S_PISTOL3 "S_PISTOL3" enf-SPR_PISG 2 4 "NULL" enf-S_PISTOL4)
    (enf--def-state states enf-S_PISTOL4 "S_PISTOL4" enf-SPR_PISG 1 5 "A_ReFire" enf-S_PISTOL)
    (enf--def-state states enf-S_PISTOLFLASH "S_PISTOLFLASH" enf-SPR_PISF 32768 7 "A_Light1" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_SGUN "S_SGUN" enf-SPR_SHTG 0 1 "A_WeaponReady" enf-S_SGUN)
    (enf--def-state states enf-S_SGUNDOWN "S_SGUNDOWN" enf-SPR_SHTG 0 1 "A_Lower" enf-S_SGUNDOWN)
    (enf--def-state states enf-S_SGUNUP "S_SGUNUP" enf-SPR_SHTG 0 1 "A_Raise" enf-S_SGUNUP)
    (enf--def-state states enf-S_SGUN1 "S_SGUN1" enf-SPR_SHTG 0 3 "NULL" enf-S_SGUN2)
    (enf--def-state states enf-S_SGUN2 "S_SGUN2" enf-SPR_SHTG 0 7 "A_FireShotgun" enf-S_SGUN3)
    (enf--def-state states enf-S_SGUN3 "S_SGUN3" enf-SPR_SHTG 1 5 "NULL" enf-S_SGUN4)
    (enf--def-state states enf-S_SGUN4 "S_SGUN4" enf-SPR_SHTG 2 5 "NULL" enf-S_SGUN5)
    (enf--def-state states enf-S_SGUN5 "S_SGUN5" enf-SPR_SHTG 3 4 "NULL" enf-S_SGUN6)
    (enf--def-state states enf-S_SGUN6 "S_SGUN6" enf-SPR_SHTG 2 5 "NULL" enf-S_SGUN7)
    (enf--def-state states enf-S_SGUN7 "S_SGUN7" enf-SPR_SHTG 1 5 "NULL" enf-S_SGUN8)
    (enf--def-state states enf-S_SGUN8 "S_SGUN8" enf-SPR_SHTG 0 3 "NULL" enf-S_SGUN9)
    (enf--def-state states enf-S_SGUN9 "S_SGUN9" enf-SPR_SHTG 0 7 "A_ReFire" enf-S_SGUN)
    (enf--def-state states enf-S_SGUNFLASH1 "S_SGUNFLASH1" enf-SPR_SHTF 32768 4 "A_Light1" enf-S_SGUNFLASH2)
    (enf--def-state states enf-S_SGUNFLASH2 "S_SGUNFLASH2" enf-SPR_SHTF 32769 3 "A_Light2" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_DSGUN "S_DSGUN" enf-SPR_SHT2 0 1 "A_WeaponReady" enf-S_DSGUN)
    (enf--def-state states enf-S_DSGUNDOWN "S_DSGUNDOWN" enf-SPR_SHT2 0 1 "A_Lower" enf-S_DSGUNDOWN)
    (enf--def-state states enf-S_DSGUNUP "S_DSGUNUP" enf-SPR_SHT2 0 1 "A_Raise" enf-S_DSGUNUP)
    (enf--def-state states enf-S_DSGUN1 "S_DSGUN1" enf-SPR_SHT2 0 3 "NULL" enf-S_DSGUN2)
    (enf--def-state states enf-S_DSGUN2 "S_DSGUN2" enf-SPR_SHT2 0 7 "A_FireShotgun2" enf-S_DSGUN3)
    (enf--def-state states enf-S_DSGUN3 "S_DSGUN3" enf-SPR_SHT2 1 7 "NULL" enf-S_DSGUN4)
    (enf--def-state states enf-S_DSGUN4 "S_DSGUN4" enf-SPR_SHT2 2 7 "A_CheckReload" enf-S_DSGUN5)
    (enf--def-state states enf-S_DSGUN5 "S_DSGUN5" enf-SPR_SHT2 3 7 "A_OpenShotgun2" enf-S_DSGUN6)
    (enf--def-state states enf-S_DSGUN6 "S_DSGUN6" enf-SPR_SHT2 4 7 "NULL" enf-S_DSGUN7)
    (enf--def-state states enf-S_DSGUN7 "S_DSGUN7" enf-SPR_SHT2 5 7 "A_LoadShotgun2" enf-S_DSGUN8)
    (enf--def-state states enf-S_DSGUN8 "S_DSGUN8" enf-SPR_SHT2 6 6 "NULL" enf-S_DSGUN9)
    (enf--def-state states enf-S_DSGUN9 "S_DSGUN9" enf-SPR_SHT2 7 6 "A_CloseShotgun2" enf-S_DSGUN10)
    (enf--def-state states enf-S_DSGUN10 "S_DSGUN10" enf-SPR_SHT2 0 5 "A_ReFire" enf-S_DSGUN)
    (enf--def-state states enf-S_DSNR1 "S_DSNR1" enf-SPR_SHT2 1 7 "NULL" enf-S_DSNR2)
    (enf--def-state states enf-S_DSNR2 "S_DSNR2" enf-SPR_SHT2 0 3 "NULL" enf-S_DSGUNDOWN)
    (enf--def-state states enf-S_DSGUNFLASH1 "S_DSGUNFLASH1" enf-SPR_SHT2 32776 5 "A_Light1" enf-S_DSGUNFLASH2)
    (enf--def-state states enf-S_DSGUNFLASH2 "S_DSGUNFLASH2" enf-SPR_SHT2 32777 4 "A_Light2" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_CHAIN "S_CHAIN" enf-SPR_CHGG 0 1 "A_WeaponReady" enf-S_CHAIN)
    (enf--def-state states enf-S_CHAINDOWN "S_CHAINDOWN" enf-SPR_CHGG 0 1 "A_Lower" enf-S_CHAINDOWN)
    (enf--def-state states enf-S_CHAINUP "S_CHAINUP" enf-SPR_CHGG 0 1 "A_Raise" enf-S_CHAINUP)
    (enf--def-state states enf-S_CHAIN1 "S_CHAIN1" enf-SPR_CHGG 0 4 "A_FireCGun" enf-S_CHAIN2)
    (enf--def-state states enf-S_CHAIN2 "S_CHAIN2" enf-SPR_CHGG 1 4 "A_FireCGun" enf-S_CHAIN3)
    (enf--def-state states enf-S_CHAIN3 "S_CHAIN3" enf-SPR_CHGG 1 0 "A_ReFire" enf-S_CHAIN)
    (enf--def-state states enf-S_CHAINFLASH1 "S_CHAINFLASH1" enf-SPR_CHGF 32768 5 "A_Light1" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_CHAINFLASH2 "S_CHAINFLASH2" enf-SPR_CHGF 32769 5 "A_Light2" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_MISSILE "S_MISSILE" enf-SPR_MISG 0 1 "A_WeaponReady" enf-S_MISSILE)
    (enf--def-state states enf-S_MISSILEDOWN "S_MISSILEDOWN" enf-SPR_MISG 0 1 "A_Lower" enf-S_MISSILEDOWN)
    (enf--def-state states enf-S_MISSILEUP "S_MISSILEUP" enf-SPR_MISG 0 1 "A_Raise" enf-S_MISSILEUP)
    (enf--def-state states enf-S_MISSILE1 "S_MISSILE1" enf-SPR_MISG 1 8 "A_GunFlash" enf-S_MISSILE2)
    (enf--def-state states enf-S_MISSILE2 "S_MISSILE2" enf-SPR_MISG 1 12 "A_FireMissile" enf-S_MISSILE3)
    (enf--def-state states enf-S_MISSILE3 "S_MISSILE3" enf-SPR_MISG 1 0 "A_ReFire" enf-S_MISSILE)
    (enf--def-state states enf-S_MISSILEFLASH1 "S_MISSILEFLASH1" enf-SPR_MISF 32768 3 "A_Light1" enf-S_MISSILEFLASH2)
    (enf--def-state states enf-S_MISSILEFLASH2 "S_MISSILEFLASH2" enf-SPR_MISF 32769 4 "NULL" enf-S_MISSILEFLASH3)
    (enf--def-state states enf-S_MISSILEFLASH3 "S_MISSILEFLASH3" enf-SPR_MISF 32770 4 "A_Light2" enf-S_MISSILEFLASH4)
    (enf--def-state states enf-S_MISSILEFLASH4 "S_MISSILEFLASH4" enf-SPR_MISF 32771 4 "A_Light2" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_SAW "S_SAW" enf-SPR_SAWG 2 4 "A_WeaponReady" enf-S_SAWB)
    (enf--def-state states enf-S_SAWB "S_SAWB" enf-SPR_SAWG 3 4 "A_WeaponReady" enf-S_SAW)
    (enf--def-state states enf-S_SAWDOWN "S_SAWDOWN" enf-SPR_SAWG 2 1 "A_Lower" enf-S_SAWDOWN)
    (enf--def-state states enf-S_SAWUP "S_SAWUP" enf-SPR_SAWG 2 1 "A_Raise" enf-S_SAWUP)
    (enf--def-state states enf-S_SAW1 "S_SAW1" enf-SPR_SAWG 0 4 "A_Saw" enf-S_SAW2)
    (enf--def-state states enf-S_SAW2 "S_SAW2" enf-SPR_SAWG 1 4 "A_Saw" enf-S_SAW3)
    (enf--def-state states enf-S_SAW3 "S_SAW3" enf-SPR_SAWG 1 0 "A_ReFire" enf-S_SAW)
    (enf--def-state states enf-S_PLASMA "S_PLASMA" enf-SPR_PLSG 0 1 "A_WeaponReady" enf-S_PLASMA)
    (enf--def-state states enf-S_PLASMADOWN "S_PLASMADOWN" enf-SPR_PLSG 0 1 "A_Lower" enf-S_PLASMADOWN)
    (enf--def-state states enf-S_PLASMAUP "S_PLASMAUP" enf-SPR_PLSG 0 1 "A_Raise" enf-S_PLASMAUP)
    (enf--def-state states enf-S_PLASMA1 "S_PLASMA1" enf-SPR_PLSG 0 3 "A_FirePlasma" enf-S_PLASMA2)
    (enf--def-state states enf-S_PLASMA2 "S_PLASMA2" enf-SPR_PLSG 1 20 "A_ReFire" enf-S_PLASMA)
    (enf--def-state states enf-S_PLASMAFLASH1 "S_PLASMAFLASH1" enf-SPR_PLSF 32768 4 "A_Light1" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_PLASMAFLASH2 "S_PLASMAFLASH2" enf-SPR_PLSF 32769 4 "A_Light1" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_BFG "S_BFG" enf-SPR_BFGG 0 1 "A_WeaponReady" enf-S_BFG)
    (enf--def-state states enf-S_BFGDOWN "S_BFGDOWN" enf-SPR_BFGG 0 1 "A_Lower" enf-S_BFGDOWN)
    (enf--def-state states enf-S_BFGUP "S_BFGUP" enf-SPR_BFGG 0 1 "A_Raise" enf-S_BFGUP)
    (enf--def-state states enf-S_BFG1 "S_BFG1" enf-SPR_BFGG 0 20 "A_BFGsound" enf-S_BFG2)
    (enf--def-state states enf-S_BFG2 "S_BFG2" enf-SPR_BFGG 1 10 "A_GunFlash" enf-S_BFG3)
    (enf--def-state states enf-S_BFG3 "S_BFG3" enf-SPR_BFGG 1 10 "A_FireBFG" enf-S_BFG4)
    (enf--def-state states enf-S_BFG4 "S_BFG4" enf-SPR_BFGG 1 20 "A_ReFire" enf-S_BFG)
    (enf--def-state states enf-S_BFGFLASH1 "S_BFGFLASH1" enf-SPR_BFGF 32768 11 "A_Light1" enf-S_BFGFLASH2)
    (enf--def-state states enf-S_BFGFLASH2 "S_BFGFLASH2" enf-SPR_BFGF 32769 6 "A_Light2" enf-S_LIGHTDONE)
    (enf--def-state states enf-S_BLOOD1 "S_BLOOD1" enf-SPR_BLUD 2 8 "NULL" enf-S_BLOOD2)
    (enf--def-state states enf-S_BLOOD2 "S_BLOOD2" enf-SPR_BLUD 1 8 "NULL" enf-S_BLOOD3)
    (enf--def-state states enf-S_BLOOD3 "S_BLOOD3" enf-SPR_BLUD 0 8 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PUFF1 "S_PUFF1" enf-SPR_PUFF 32768 4 "NULL" enf-S_PUFF2)
    (enf--def-state states enf-S_PUFF2 "S_PUFF2" enf-SPR_PUFF 1 4 "NULL" enf-S_PUFF3)
    (enf--def-state states enf-S_PUFF3 "S_PUFF3" enf-SPR_PUFF 2 4 "NULL" enf-S_PUFF4)
    (enf--def-state states enf-S_PUFF4 "S_PUFF4" enf-SPR_PUFF 3 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TBALL1 "S_TBALL1" enf-SPR_BAL1 32768 4 "NULL" enf-S_TBALL2)
    (enf--def-state states enf-S_TBALL2 "S_TBALL2" enf-SPR_BAL1 32769 4 "NULL" enf-S_TBALL1)
    (enf--def-state states enf-S_TBALLX1 "S_TBALLX1" enf-SPR_BAL1 32770 6 "NULL" enf-S_TBALLX2)
    (enf--def-state states enf-S_TBALLX2 "S_TBALLX2" enf-SPR_BAL1 32771 6 "NULL" enf-S_TBALLX3)
    (enf--def-state states enf-S_TBALLX3 "S_TBALLX3" enf-SPR_BAL1 32772 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_RBALL1 "S_RBALL1" enf-SPR_BAL2 32768 4 "NULL" enf-S_RBALL2)
    (enf--def-state states enf-S_RBALL2 "S_RBALL2" enf-SPR_BAL2 32769 4 "NULL" enf-S_RBALL1)
    (enf--def-state states enf-S_RBALLX1 "S_RBALLX1" enf-SPR_BAL2 32770 6 "NULL" enf-S_RBALLX2)
    (enf--def-state states enf-S_RBALLX2 "S_RBALLX2" enf-SPR_BAL2 32771 6 "NULL" enf-S_RBALLX3)
    (enf--def-state states enf-S_RBALLX3 "S_RBALLX3" enf-SPR_BAL2 32772 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PLASBALL "S_PLASBALL" enf-SPR_PLSS 32768 6 "NULL" enf-S_PLASBALL2)
    (enf--def-state states enf-S_PLASBALL2 "S_PLASBALL2" enf-SPR_PLSS 32769 6 "NULL" enf-S_PLASBALL)
    (enf--def-state states enf-S_PLASEXP "S_PLASEXP" enf-SPR_PLSE 32768 4 "NULL" enf-S_PLASEXP2)
    (enf--def-state states enf-S_PLASEXP2 "S_PLASEXP2" enf-SPR_PLSE 32769 4 "NULL" enf-S_PLASEXP3)
    (enf--def-state states enf-S_PLASEXP3 "S_PLASEXP3" enf-SPR_PLSE 32770 4 "NULL" enf-S_PLASEXP4)
    (enf--def-state states enf-S_PLASEXP4 "S_PLASEXP4" enf-SPR_PLSE 32771 4 "NULL" enf-S_PLASEXP5)
    (enf--def-state states enf-S_PLASEXP5 "S_PLASEXP5" enf-SPR_PLSE 32772 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_ROCKET "S_ROCKET" enf-SPR_MISL 32768 1 "NULL" enf-S_ROCKET)
    (enf--def-state states enf-S_BFGSHOT "S_BFGSHOT" enf-SPR_BFS1 32768 4 "NULL" enf-S_BFGSHOT2)
    (enf--def-state states enf-S_BFGSHOT2 "S_BFGSHOT2" enf-SPR_BFS1 32769 4 "NULL" enf-S_BFGSHOT)
    (enf--def-state states enf-S_BFGLAND "S_BFGLAND" enf-SPR_BFE1 32768 8 "NULL" enf-S_BFGLAND2)
    (enf--def-state states enf-S_BFGLAND2 "S_BFGLAND2" enf-SPR_BFE1 32769 8 "NULL" enf-S_BFGLAND3)
    (enf--def-state states enf-S_BFGLAND3 "S_BFGLAND3" enf-SPR_BFE1 32770 8 "A_BFGSpray" enf-S_BFGLAND4)
    (enf--def-state states enf-S_BFGLAND4 "S_BFGLAND4" enf-SPR_BFE1 32771 8 "NULL" enf-S_BFGLAND5)
    (enf--def-state states enf-S_BFGLAND5 "S_BFGLAND5" enf-SPR_BFE1 32772 8 "NULL" enf-S_BFGLAND6)
    (enf--def-state states enf-S_BFGLAND6 "S_BFGLAND6" enf-SPR_BFE1 32773 8 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BFGEXP "S_BFGEXP" enf-SPR_BFE2 32768 8 "NULL" enf-S_BFGEXP2)
    (enf--def-state states enf-S_BFGEXP2 "S_BFGEXP2" enf-SPR_BFE2 32769 8 "NULL" enf-S_BFGEXP3)
    (enf--def-state states enf-S_BFGEXP3 "S_BFGEXP3" enf-SPR_BFE2 32770 8 "NULL" enf-S_BFGEXP4)
    (enf--def-state states enf-S_BFGEXP4 "S_BFGEXP4" enf-SPR_BFE2 32771 8 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_EXPLODE1 "S_EXPLODE1" enf-SPR_MISL 32769 8 "A_Explode" enf-S_EXPLODE2)
    (enf--def-state states enf-S_EXPLODE2 "S_EXPLODE2" enf-SPR_MISL 32770 6 "NULL" enf-S_EXPLODE3)
    (enf--def-state states enf-S_EXPLODE3 "S_EXPLODE3" enf-SPR_MISL 32771 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TFOG "S_TFOG" enf-SPR_TFOG 32768 6 "NULL" enf-S_TFOG01)
    (enf--def-state states enf-S_TFOG01 "S_TFOG01" enf-SPR_TFOG 32769 6 "NULL" enf-S_TFOG02)
    (enf--def-state states enf-S_TFOG02 "S_TFOG02" enf-SPR_TFOG 32768 6 "NULL" enf-S_TFOG2)
    (enf--def-state states enf-S_TFOG2 "S_TFOG2" enf-SPR_TFOG 32769 6 "NULL" enf-S_TFOG3)
    (enf--def-state states enf-S_TFOG3 "S_TFOG3" enf-SPR_TFOG 32770 6 "NULL" enf-S_TFOG4)
    (enf--def-state states enf-S_TFOG4 "S_TFOG4" enf-SPR_TFOG 32771 6 "NULL" enf-S_TFOG5)
    (enf--def-state states enf-S_TFOG5 "S_TFOG5" enf-SPR_TFOG 32772 6 "NULL" enf-S_TFOG6)
    (enf--def-state states enf-S_TFOG6 "S_TFOG6" enf-SPR_TFOG 32773 6 "NULL" enf-S_TFOG7)
    (enf--def-state states enf-S_TFOG7 "S_TFOG7" enf-SPR_TFOG 32774 6 "NULL" enf-S_TFOG8)
    (enf--def-state states enf-S_TFOG8 "S_TFOG8" enf-SPR_TFOG 32775 6 "NULL" enf-S_TFOG9)
    (enf--def-state states enf-S_TFOG9 "S_TFOG9" enf-SPR_TFOG 32776 6 "NULL" enf-S_TFOG10)
    (enf--def-state states enf-S_TFOG10 "S_TFOG10" enf-SPR_TFOG 32777 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_IFOG "S_IFOG" enf-SPR_IFOG 32768 6 "NULL" enf-S_IFOG01)
    (enf--def-state states enf-S_IFOG01 "S_IFOG01" enf-SPR_IFOG 32769 6 "NULL" enf-S_IFOG02)
    (enf--def-state states enf-S_IFOG02 "S_IFOG02" enf-SPR_IFOG 32768 6 "NULL" enf-S_IFOG2)
    (enf--def-state states enf-S_IFOG2 "S_IFOG2" enf-SPR_IFOG 32769 6 "NULL" enf-S_IFOG3)
    (enf--def-state states enf-S_IFOG3 "S_IFOG3" enf-SPR_IFOG 32770 6 "NULL" enf-S_IFOG4)
    (enf--def-state states enf-S_IFOG4 "S_IFOG4" enf-SPR_IFOG 32771 6 "NULL" enf-S_IFOG5)
    (enf--def-state states enf-S_IFOG5 "S_IFOG5" enf-SPR_IFOG 32772 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PLAY "S_PLAY" enf-SPR_PLAY 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PLAY_RUN1 "S_PLAY_RUN1" enf-SPR_PLAY 0 4 "NULL" enf-S_PLAY_RUN2)
    (enf--def-state states enf-S_PLAY_RUN2 "S_PLAY_RUN2" enf-SPR_PLAY 1 4 "NULL" enf-S_PLAY_RUN3)
    (enf--def-state states enf-S_PLAY_RUN3 "S_PLAY_RUN3" enf-SPR_PLAY 2 4 "NULL" enf-S_PLAY_RUN4)
    (enf--def-state states enf-S_PLAY_RUN4 "S_PLAY_RUN4" enf-SPR_PLAY 3 4 "NULL" enf-S_PLAY_RUN1)
    (enf--def-state states enf-S_PLAY_ATK1 "S_PLAY_ATK1" enf-SPR_PLAY 4 12 "NULL" enf-S_PLAY)
    (enf--def-state states enf-S_PLAY_ATK2 "S_PLAY_ATK2" enf-SPR_PLAY 32773 6 "NULL" enf-S_PLAY_ATK1)
    (enf--def-state states enf-S_PLAY_PAIN "S_PLAY_PAIN" enf-SPR_PLAY 6 4 "NULL" enf-S_PLAY_PAIN2)
    (enf--def-state states enf-S_PLAY_PAIN2 "S_PLAY_PAIN2" enf-SPR_PLAY 6 4 "A_Pain" enf-S_PLAY)
    (enf--def-state states enf-S_PLAY_DIE1 "S_PLAY_DIE1" enf-SPR_PLAY 7 10 "NULL" enf-S_PLAY_DIE2)
    (enf--def-state states enf-S_PLAY_DIE2 "S_PLAY_DIE2" enf-SPR_PLAY 8 10 "A_PlayerScream" enf-S_PLAY_DIE3)
    (enf--def-state states enf-S_PLAY_DIE3 "S_PLAY_DIE3" enf-SPR_PLAY 9 10 "A_Fall" enf-S_PLAY_DIE4)
    (enf--def-state states enf-S_PLAY_DIE4 "S_PLAY_DIE4" enf-SPR_PLAY 10 10 "NULL" enf-S_PLAY_DIE5)
    (enf--def-state states enf-S_PLAY_DIE5 "S_PLAY_DIE5" enf-SPR_PLAY 11 10 "NULL" enf-S_PLAY_DIE6)
    (enf--def-state states enf-S_PLAY_DIE6 "S_PLAY_DIE6" enf-SPR_PLAY 12 10 "NULL" enf-S_PLAY_DIE7)
    (enf--def-state states enf-S_PLAY_DIE7 "S_PLAY_DIE7" enf-SPR_PLAY 13 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PLAY_XDIE1 "S_PLAY_XDIE1" enf-SPR_PLAY 14 5 "NULL" enf-S_PLAY_XDIE2)
    (enf--def-state states enf-S_PLAY_XDIE2 "S_PLAY_XDIE2" enf-SPR_PLAY 15 5 "A_XScream" enf-S_PLAY_XDIE3)
    (enf--def-state states enf-S_PLAY_XDIE3 "S_PLAY_XDIE3" enf-SPR_PLAY 16 5 "A_Fall" enf-S_PLAY_XDIE4)
    (enf--def-state states enf-S_PLAY_XDIE4 "S_PLAY_XDIE4" enf-SPR_PLAY 17 5 "NULL" enf-S_PLAY_XDIE5)
    (enf--def-state states enf-S_PLAY_XDIE5 "S_PLAY_XDIE5" enf-SPR_PLAY 18 5 "NULL" enf-S_PLAY_XDIE6)
    (enf--def-state states enf-S_PLAY_XDIE6 "S_PLAY_XDIE6" enf-SPR_PLAY 19 5 "NULL" enf-S_PLAY_XDIE7)
    (enf--def-state states enf-S_PLAY_XDIE7 "S_PLAY_XDIE7" enf-SPR_PLAY 20 5 "NULL" enf-S_PLAY_XDIE8)
    (enf--def-state states enf-S_PLAY_XDIE8 "S_PLAY_XDIE8" enf-SPR_PLAY 21 5 "NULL" enf-S_PLAY_XDIE9)
    (enf--def-state states enf-S_PLAY_XDIE9 "S_PLAY_XDIE9" enf-SPR_PLAY 22 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_POSS_STND "S_POSS_STND" enf-SPR_POSS 0 10 "A_Look" enf-S_POSS_STND2)
    (enf--def-state states enf-S_POSS_STND2 "S_POSS_STND2" enf-SPR_POSS 1 10 "A_Look" enf-S_POSS_STND)
    (enf--def-state states enf-S_POSS_RUN1 "S_POSS_RUN1" enf-SPR_POSS 0 4 "A_Chase" enf-S_POSS_RUN2)
    (enf--def-state states enf-S_POSS_RUN2 "S_POSS_RUN2" enf-SPR_POSS 0 4 "A_Chase" enf-S_POSS_RUN3)
    (enf--def-state states enf-S_POSS_RUN3 "S_POSS_RUN3" enf-SPR_POSS 1 4 "A_Chase" enf-S_POSS_RUN4)
    (enf--def-state states enf-S_POSS_RUN4 "S_POSS_RUN4" enf-SPR_POSS 1 4 "A_Chase" enf-S_POSS_RUN5)
    (enf--def-state states enf-S_POSS_RUN5 "S_POSS_RUN5" enf-SPR_POSS 2 4 "A_Chase" enf-S_POSS_RUN6)
    (enf--def-state states enf-S_POSS_RUN6 "S_POSS_RUN6" enf-SPR_POSS 2 4 "A_Chase" enf-S_POSS_RUN7)
    (enf--def-state states enf-S_POSS_RUN7 "S_POSS_RUN7" enf-SPR_POSS 3 4 "A_Chase" enf-S_POSS_RUN8)
    (enf--def-state states enf-S_POSS_RUN8 "S_POSS_RUN8" enf-SPR_POSS 3 4 "A_Chase" enf-S_POSS_RUN1)
    (enf--def-state states enf-S_POSS_ATK1 "S_POSS_ATK1" enf-SPR_POSS 4 10 "A_FaceTarget" enf-S_POSS_ATK2)
    (enf--def-state states enf-S_POSS_ATK2 "S_POSS_ATK2" enf-SPR_POSS 5 8 "A_PosAttack" enf-S_POSS_ATK3)
    (enf--def-state states enf-S_POSS_ATK3 "S_POSS_ATK3" enf-SPR_POSS 4 8 "NULL" enf-S_POSS_RUN1)
    (enf--def-state states enf-S_POSS_PAIN "S_POSS_PAIN" enf-SPR_POSS 6 3 "NULL" enf-S_POSS_PAIN2)
    (enf--def-state states enf-S_POSS_PAIN2 "S_POSS_PAIN2" enf-SPR_POSS 6 3 "A_Pain" enf-S_POSS_RUN1)
    (enf--def-state states enf-S_POSS_DIE1 "S_POSS_DIE1" enf-SPR_POSS 7 5 "NULL" enf-S_POSS_DIE2)
    (enf--def-state states enf-S_POSS_DIE2 "S_POSS_DIE2" enf-SPR_POSS 8 5 "A_Scream" enf-S_POSS_DIE3)
    (enf--def-state states enf-S_POSS_DIE3 "S_POSS_DIE3" enf-SPR_POSS 9 5 "A_Fall" enf-S_POSS_DIE4)
    (enf--def-state states enf-S_POSS_DIE4 "S_POSS_DIE4" enf-SPR_POSS 10 5 "NULL" enf-S_POSS_DIE5)
    (enf--def-state states enf-S_POSS_DIE5 "S_POSS_DIE5" enf-SPR_POSS 11 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_POSS_XDIE1 "S_POSS_XDIE1" enf-SPR_POSS 12 5 "NULL" enf-S_POSS_XDIE2)
    (enf--def-state states enf-S_POSS_XDIE2 "S_POSS_XDIE2" enf-SPR_POSS 13 5 "A_XScream" enf-S_POSS_XDIE3)
    (enf--def-state states enf-S_POSS_XDIE3 "S_POSS_XDIE3" enf-SPR_POSS 14 5 "A_Fall" enf-S_POSS_XDIE4)
    (enf--def-state states enf-S_POSS_XDIE4 "S_POSS_XDIE4" enf-SPR_POSS 15 5 "NULL" enf-S_POSS_XDIE5)
    (enf--def-state states enf-S_POSS_XDIE5 "S_POSS_XDIE5" enf-SPR_POSS 16 5 "NULL" enf-S_POSS_XDIE6)
    (enf--def-state states enf-S_POSS_XDIE6 "S_POSS_XDIE6" enf-SPR_POSS 17 5 "NULL" enf-S_POSS_XDIE7)
    (enf--def-state states enf-S_POSS_XDIE7 "S_POSS_XDIE7" enf-SPR_POSS 18 5 "NULL" enf-S_POSS_XDIE8)
    (enf--def-state states enf-S_POSS_XDIE8 "S_POSS_XDIE8" enf-SPR_POSS 19 5 "NULL" enf-S_POSS_XDIE9)
    (enf--def-state states enf-S_POSS_XDIE9 "S_POSS_XDIE9" enf-SPR_POSS 20 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_POSS_RAISE1 "S_POSS_RAISE1" enf-SPR_POSS 10 5 "NULL" enf-S_POSS_RAISE2)
    (enf--def-state states enf-S_POSS_RAISE2 "S_POSS_RAISE2" enf-SPR_POSS 9 5 "NULL" enf-S_POSS_RAISE3)
    (enf--def-state states enf-S_POSS_RAISE3 "S_POSS_RAISE3" enf-SPR_POSS 8 5 "NULL" enf-S_POSS_RAISE4)
    (enf--def-state states enf-S_POSS_RAISE4 "S_POSS_RAISE4" enf-SPR_POSS 7 5 "NULL" enf-S_POSS_RUN1)
    (enf--def-state states enf-S_SPOS_STND "S_SPOS_STND" enf-SPR_SPOS 0 10 "A_Look" enf-S_SPOS_STND2)
    (enf--def-state states enf-S_SPOS_STND2 "S_SPOS_STND2" enf-SPR_SPOS 1 10 "A_Look" enf-S_SPOS_STND)
    (enf--def-state states enf-S_SPOS_RUN1 "S_SPOS_RUN1" enf-SPR_SPOS 0 3 "A_Chase" enf-S_SPOS_RUN2)
    (enf--def-state states enf-S_SPOS_RUN2 "S_SPOS_RUN2" enf-SPR_SPOS 0 3 "A_Chase" enf-S_SPOS_RUN3)
    (enf--def-state states enf-S_SPOS_RUN3 "S_SPOS_RUN3" enf-SPR_SPOS 1 3 "A_Chase" enf-S_SPOS_RUN4)
    (enf--def-state states enf-S_SPOS_RUN4 "S_SPOS_RUN4" enf-SPR_SPOS 1 3 "A_Chase" enf-S_SPOS_RUN5)
    (enf--def-state states enf-S_SPOS_RUN5 "S_SPOS_RUN5" enf-SPR_SPOS 2 3 "A_Chase" enf-S_SPOS_RUN6)
    (enf--def-state states enf-S_SPOS_RUN6 "S_SPOS_RUN6" enf-SPR_SPOS 2 3 "A_Chase" enf-S_SPOS_RUN7)
    (enf--def-state states enf-S_SPOS_RUN7 "S_SPOS_RUN7" enf-SPR_SPOS 3 3 "A_Chase" enf-S_SPOS_RUN8)
    (enf--def-state states enf-S_SPOS_RUN8 "S_SPOS_RUN8" enf-SPR_SPOS 3 3 "A_Chase" enf-S_SPOS_RUN1)
    (enf--def-state states enf-S_SPOS_ATK1 "S_SPOS_ATK1" enf-SPR_SPOS 4 10 "A_FaceTarget" enf-S_SPOS_ATK2)
    (enf--def-state states enf-S_SPOS_ATK2 "S_SPOS_ATK2" enf-SPR_SPOS 32773 10 "A_SPosAttack" enf-S_SPOS_ATK3)
    (enf--def-state states enf-S_SPOS_ATK3 "S_SPOS_ATK3" enf-SPR_SPOS 4 10 "NULL" enf-S_SPOS_RUN1)
    (enf--def-state states enf-S_SPOS_PAIN "S_SPOS_PAIN" enf-SPR_SPOS 6 3 "NULL" enf-S_SPOS_PAIN2)
    (enf--def-state states enf-S_SPOS_PAIN2 "S_SPOS_PAIN2" enf-SPR_SPOS 6 3 "A_Pain" enf-S_SPOS_RUN1)
    (enf--def-state states enf-S_SPOS_DIE1 "S_SPOS_DIE1" enf-SPR_SPOS 7 5 "NULL" enf-S_SPOS_DIE2)
    (enf--def-state states enf-S_SPOS_DIE2 "S_SPOS_DIE2" enf-SPR_SPOS 8 5 "A_Scream" enf-S_SPOS_DIE3)
    (enf--def-state states enf-S_SPOS_DIE3 "S_SPOS_DIE3" enf-SPR_SPOS 9 5 "A_Fall" enf-S_SPOS_DIE4)
    (enf--def-state states enf-S_SPOS_DIE4 "S_SPOS_DIE4" enf-SPR_SPOS 10 5 "NULL" enf-S_SPOS_DIE5)
    (enf--def-state states enf-S_SPOS_DIE5 "S_SPOS_DIE5" enf-SPR_SPOS 11 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SPOS_XDIE1 "S_SPOS_XDIE1" enf-SPR_SPOS 12 5 "NULL" enf-S_SPOS_XDIE2)
    (enf--def-state states enf-S_SPOS_XDIE2 "S_SPOS_XDIE2" enf-SPR_SPOS 13 5 "A_XScream" enf-S_SPOS_XDIE3)
    (enf--def-state states enf-S_SPOS_XDIE3 "S_SPOS_XDIE3" enf-SPR_SPOS 14 5 "A_Fall" enf-S_SPOS_XDIE4)
    (enf--def-state states enf-S_SPOS_XDIE4 "S_SPOS_XDIE4" enf-SPR_SPOS 15 5 "NULL" enf-S_SPOS_XDIE5)
    (enf--def-state states enf-S_SPOS_XDIE5 "S_SPOS_XDIE5" enf-SPR_SPOS 16 5 "NULL" enf-S_SPOS_XDIE6)
    (enf--def-state states enf-S_SPOS_XDIE6 "S_SPOS_XDIE6" enf-SPR_SPOS 17 5 "NULL" enf-S_SPOS_XDIE7)
    (enf--def-state states enf-S_SPOS_XDIE7 "S_SPOS_XDIE7" enf-SPR_SPOS 18 5 "NULL" enf-S_SPOS_XDIE8)
    (enf--def-state states enf-S_SPOS_XDIE8 "S_SPOS_XDIE8" enf-SPR_SPOS 19 5 "NULL" enf-S_SPOS_XDIE9)
    (enf--def-state states enf-S_SPOS_XDIE9 "S_SPOS_XDIE9" enf-SPR_SPOS 20 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SPOS_RAISE1 "S_SPOS_RAISE1" enf-SPR_SPOS 11 5 "NULL" enf-S_SPOS_RAISE2)
    (enf--def-state states enf-S_SPOS_RAISE2 "S_SPOS_RAISE2" enf-SPR_SPOS 10 5 "NULL" enf-S_SPOS_RAISE3)
    (enf--def-state states enf-S_SPOS_RAISE3 "S_SPOS_RAISE3" enf-SPR_SPOS 9 5 "NULL" enf-S_SPOS_RAISE4)
    (enf--def-state states enf-S_SPOS_RAISE4 "S_SPOS_RAISE4" enf-SPR_SPOS 8 5 "NULL" enf-S_SPOS_RAISE5)
    (enf--def-state states enf-S_SPOS_RAISE5 "S_SPOS_RAISE5" enf-SPR_SPOS 7 5 "NULL" enf-S_SPOS_RUN1)
    (enf--def-state states enf-S_VILE_STND "S_VILE_STND" enf-SPR_VILE 0 10 "A_Look" enf-S_VILE_STND2)
    (enf--def-state states enf-S_VILE_STND2 "S_VILE_STND2" enf-SPR_VILE 1 10 "A_Look" enf-S_VILE_STND)
    (enf--def-state states enf-S_VILE_RUN1 "S_VILE_RUN1" enf-SPR_VILE 0 2 "A_VileChase" enf-S_VILE_RUN2)
    (enf--def-state states enf-S_VILE_RUN2 "S_VILE_RUN2" enf-SPR_VILE 0 2 "A_VileChase" enf-S_VILE_RUN3)
    (enf--def-state states enf-S_VILE_RUN3 "S_VILE_RUN3" enf-SPR_VILE 1 2 "A_VileChase" enf-S_VILE_RUN4)
    (enf--def-state states enf-S_VILE_RUN4 "S_VILE_RUN4" enf-SPR_VILE 1 2 "A_VileChase" enf-S_VILE_RUN5)
    (enf--def-state states enf-S_VILE_RUN5 "S_VILE_RUN5" enf-SPR_VILE 2 2 "A_VileChase" enf-S_VILE_RUN6)
    (enf--def-state states enf-S_VILE_RUN6 "S_VILE_RUN6" enf-SPR_VILE 2 2 "A_VileChase" enf-S_VILE_RUN7)
    (enf--def-state states enf-S_VILE_RUN7 "S_VILE_RUN7" enf-SPR_VILE 3 2 "A_VileChase" enf-S_VILE_RUN8)
    (enf--def-state states enf-S_VILE_RUN8 "S_VILE_RUN8" enf-SPR_VILE 3 2 "A_VileChase" enf-S_VILE_RUN9)
    (enf--def-state states enf-S_VILE_RUN9 "S_VILE_RUN9" enf-SPR_VILE 4 2 "A_VileChase" enf-S_VILE_RUN10)
    (enf--def-state states enf-S_VILE_RUN10 "S_VILE_RUN10" enf-SPR_VILE 4 2 "A_VileChase" enf-S_VILE_RUN11)
    (enf--def-state states enf-S_VILE_RUN11 "S_VILE_RUN11" enf-SPR_VILE 5 2 "A_VileChase" enf-S_VILE_RUN12)
    (enf--def-state states enf-S_VILE_RUN12 "S_VILE_RUN12" enf-SPR_VILE 5 2 "A_VileChase" enf-S_VILE_RUN1)
    (enf--def-state states enf-S_VILE_ATK1 "S_VILE_ATK1" enf-SPR_VILE 32774 0 "A_VileStart" enf-S_VILE_ATK2)
    (enf--def-state states enf-S_VILE_ATK2 "S_VILE_ATK2" enf-SPR_VILE 32774 10 "A_FaceTarget" enf-S_VILE_ATK3)
    (enf--def-state states enf-S_VILE_ATK3 "S_VILE_ATK3" enf-SPR_VILE 32775 8 "A_VileTarget" enf-S_VILE_ATK4)
    (enf--def-state states enf-S_VILE_ATK4 "S_VILE_ATK4" enf-SPR_VILE 32776 8 "A_FaceTarget" enf-S_VILE_ATK5)
    (enf--def-state states enf-S_VILE_ATK5 "S_VILE_ATK5" enf-SPR_VILE 32777 8 "A_FaceTarget" enf-S_VILE_ATK6)
    (enf--def-state states enf-S_VILE_ATK6 "S_VILE_ATK6" enf-SPR_VILE 32778 8 "A_FaceTarget" enf-S_VILE_ATK7)
    (enf--def-state states enf-S_VILE_ATK7 "S_VILE_ATK7" enf-SPR_VILE 32779 8 "A_FaceTarget" enf-S_VILE_ATK8)
    (enf--def-state states enf-S_VILE_ATK8 "S_VILE_ATK8" enf-SPR_VILE 32780 8 "A_FaceTarget" enf-S_VILE_ATK9)
    (enf--def-state states enf-S_VILE_ATK9 "S_VILE_ATK9" enf-SPR_VILE 32781 8 "A_FaceTarget" enf-S_VILE_ATK10)
    (enf--def-state states enf-S_VILE_ATK10 "S_VILE_ATK10" enf-SPR_VILE 32782 8 "A_VileAttack" enf-S_VILE_ATK11)
    (enf--def-state states enf-S_VILE_ATK11 "S_VILE_ATK11" enf-SPR_VILE 32783 20 "NULL" enf-S_VILE_RUN1)
    (enf--def-state states enf-S_VILE_HEAL1 "S_VILE_HEAL1" enf-SPR_VILE 32794 10 "NULL" enf-S_VILE_HEAL2)
    (enf--def-state states enf-S_VILE_HEAL2 "S_VILE_HEAL2" enf-SPR_VILE 32795 10 "NULL" enf-S_VILE_HEAL3)
    (enf--def-state states enf-S_VILE_HEAL3 "S_VILE_HEAL3" enf-SPR_VILE 32796 10 "NULL" enf-S_VILE_RUN1)
    (enf--def-state states enf-S_VILE_PAIN "S_VILE_PAIN" enf-SPR_VILE 16 5 "NULL" enf-S_VILE_PAIN2)
    (enf--def-state states enf-S_VILE_PAIN2 "S_VILE_PAIN2" enf-SPR_VILE 16 5 "A_Pain" enf-S_VILE_RUN1)
    (enf--def-state states enf-S_VILE_DIE1 "S_VILE_DIE1" enf-SPR_VILE 16 7 "NULL" enf-S_VILE_DIE2)
    (enf--def-state states enf-S_VILE_DIE2 "S_VILE_DIE2" enf-SPR_VILE 17 7 "A_Scream" enf-S_VILE_DIE3)
    (enf--def-state states enf-S_VILE_DIE3 "S_VILE_DIE3" enf-SPR_VILE 18 7 "A_Fall" enf-S_VILE_DIE4)
    (enf--def-state states enf-S_VILE_DIE4 "S_VILE_DIE4" enf-SPR_VILE 19 7 "NULL" enf-S_VILE_DIE5)
    (enf--def-state states enf-S_VILE_DIE5 "S_VILE_DIE5" enf-SPR_VILE 20 7 "NULL" enf-S_VILE_DIE6)
    (enf--def-state states enf-S_VILE_DIE6 "S_VILE_DIE6" enf-SPR_VILE 21 7 "NULL" enf-S_VILE_DIE7)
    (enf--def-state states enf-S_VILE_DIE7 "S_VILE_DIE7" enf-SPR_VILE 22 7 "NULL" enf-S_VILE_DIE8)
    (enf--def-state states enf-S_VILE_DIE8 "S_VILE_DIE8" enf-SPR_VILE 23 5 "NULL" enf-S_VILE_DIE9)
    (enf--def-state states enf-S_VILE_DIE9 "S_VILE_DIE9" enf-SPR_VILE 24 5 "NULL" enf-S_VILE_DIE10)
    (enf--def-state states enf-S_VILE_DIE10 "S_VILE_DIE10" enf-SPR_VILE 25 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_FIRE1 "S_FIRE1" enf-SPR_FIRE 32768 2 "A_StartFire" enf-S_FIRE2)
    (enf--def-state states enf-S_FIRE2 "S_FIRE2" enf-SPR_FIRE 32769 2 "A_Fire" enf-S_FIRE3)
    (enf--def-state states enf-S_FIRE3 "S_FIRE3" enf-SPR_FIRE 32768 2 "A_Fire" enf-S_FIRE4)
    (enf--def-state states enf-S_FIRE4 "S_FIRE4" enf-SPR_FIRE 32769 2 "A_Fire" enf-S_FIRE5)
    (enf--def-state states enf-S_FIRE5 "S_FIRE5" enf-SPR_FIRE 32770 2 "A_FireCrackle" enf-S_FIRE6)
    (enf--def-state states enf-S_FIRE6 "S_FIRE6" enf-SPR_FIRE 32769 2 "A_Fire" enf-S_FIRE7)
    (enf--def-state states enf-S_FIRE7 "S_FIRE7" enf-SPR_FIRE 32770 2 "A_Fire" enf-S_FIRE8)
    (enf--def-state states enf-S_FIRE8 "S_FIRE8" enf-SPR_FIRE 32769 2 "A_Fire" enf-S_FIRE9)
    (enf--def-state states enf-S_FIRE9 "S_FIRE9" enf-SPR_FIRE 32770 2 "A_Fire" enf-S_FIRE10)
    (enf--def-state states enf-S_FIRE10 "S_FIRE10" enf-SPR_FIRE 32771 2 "A_Fire" enf-S_FIRE11)
    (enf--def-state states enf-S_FIRE11 "S_FIRE11" enf-SPR_FIRE 32770 2 "A_Fire" enf-S_FIRE12)
    (enf--def-state states enf-S_FIRE12 "S_FIRE12" enf-SPR_FIRE 32771 2 "A_Fire" enf-S_FIRE13)
    (enf--def-state states enf-S_FIRE13 "S_FIRE13" enf-SPR_FIRE 32770 2 "A_Fire" enf-S_FIRE14)
    (enf--def-state states enf-S_FIRE14 "S_FIRE14" enf-SPR_FIRE 32771 2 "A_Fire" enf-S_FIRE15)
    (enf--def-state states enf-S_FIRE15 "S_FIRE15" enf-SPR_FIRE 32772 2 "A_Fire" enf-S_FIRE16)
    (enf--def-state states enf-S_FIRE16 "S_FIRE16" enf-SPR_FIRE 32771 2 "A_Fire" enf-S_FIRE17)
    (enf--def-state states enf-S_FIRE17 "S_FIRE17" enf-SPR_FIRE 32772 2 "A_Fire" enf-S_FIRE18)
    (enf--def-state states enf-S_FIRE18 "S_FIRE18" enf-SPR_FIRE 32771 2 "A_Fire" enf-S_FIRE19)
    (enf--def-state states enf-S_FIRE19 "S_FIRE19" enf-SPR_FIRE 32772 2 "A_FireCrackle" enf-S_FIRE20)
    (enf--def-state states enf-S_FIRE20 "S_FIRE20" enf-SPR_FIRE 32773 2 "A_Fire" enf-S_FIRE21)
    (enf--def-state states enf-S_FIRE21 "S_FIRE21" enf-SPR_FIRE 32772 2 "A_Fire" enf-S_FIRE22)
    (enf--def-state states enf-S_FIRE22 "S_FIRE22" enf-SPR_FIRE 32773 2 "A_Fire" enf-S_FIRE23)
    (enf--def-state states enf-S_FIRE23 "S_FIRE23" enf-SPR_FIRE 32772 2 "A_Fire" enf-S_FIRE24)
    (enf--def-state states enf-S_FIRE24 "S_FIRE24" enf-SPR_FIRE 32773 2 "A_Fire" enf-S_FIRE25)
    (enf--def-state states enf-S_FIRE25 "S_FIRE25" enf-SPR_FIRE 32774 2 "A_Fire" enf-S_FIRE26)
    (enf--def-state states enf-S_FIRE26 "S_FIRE26" enf-SPR_FIRE 32775 2 "A_Fire" enf-S_FIRE27)
    (enf--def-state states enf-S_FIRE27 "S_FIRE27" enf-SPR_FIRE 32774 2 "A_Fire" enf-S_FIRE28)
    (enf--def-state states enf-S_FIRE28 "S_FIRE28" enf-SPR_FIRE 32775 2 "A_Fire" enf-S_FIRE29)
    (enf--def-state states enf-S_FIRE29 "S_FIRE29" enf-SPR_FIRE 32774 2 "A_Fire" enf-S_FIRE30)
    (enf--def-state states enf-S_FIRE30 "S_FIRE30" enf-SPR_FIRE 32775 2 "A_Fire" enf-S_NULL)
    (enf--def-state states enf-S_SMOKE1 "S_SMOKE1" enf-SPR_PUFF 1 4 "NULL" enf-S_SMOKE2)
    (enf--def-state states enf-S_SMOKE2 "S_SMOKE2" enf-SPR_PUFF 2 4 "NULL" enf-S_SMOKE3)
    (enf--def-state states enf-S_SMOKE3 "S_SMOKE3" enf-SPR_PUFF 1 4 "NULL" enf-S_SMOKE4)
    (enf--def-state states enf-S_SMOKE4 "S_SMOKE4" enf-SPR_PUFF 2 4 "NULL" enf-S_SMOKE5)
    (enf--def-state states enf-S_SMOKE5 "S_SMOKE5" enf-SPR_PUFF 3 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TRACER "S_TRACER" enf-SPR_FATB 32768 2 "A_Tracer" enf-S_TRACER2)
    (enf--def-state states enf-S_TRACER2 "S_TRACER2" enf-SPR_FATB 32769 2 "A_Tracer" enf-S_TRACER)
    (enf--def-state states enf-S_TRACEEXP1 "S_TRACEEXP1" enf-SPR_FBXP 32768 8 "NULL" enf-S_TRACEEXP2)
    (enf--def-state states enf-S_TRACEEXP2 "S_TRACEEXP2" enf-SPR_FBXP 32769 6 "NULL" enf-S_TRACEEXP3)
    (enf--def-state states enf-S_TRACEEXP3 "S_TRACEEXP3" enf-SPR_FBXP 32770 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SKEL_STND "S_SKEL_STND" enf-SPR_SKEL 0 10 "A_Look" enf-S_SKEL_STND2)
    (enf--def-state states enf-S_SKEL_STND2 "S_SKEL_STND2" enf-SPR_SKEL 1 10 "A_Look" enf-S_SKEL_STND)
    (enf--def-state states enf-S_SKEL_RUN1 "S_SKEL_RUN1" enf-SPR_SKEL 0 2 "A_Chase" enf-S_SKEL_RUN2)
    (enf--def-state states enf-S_SKEL_RUN2 "S_SKEL_RUN2" enf-SPR_SKEL 0 2 "A_Chase" enf-S_SKEL_RUN3)
    (enf--def-state states enf-S_SKEL_RUN3 "S_SKEL_RUN3" enf-SPR_SKEL 1 2 "A_Chase" enf-S_SKEL_RUN4)
    (enf--def-state states enf-S_SKEL_RUN4 "S_SKEL_RUN4" enf-SPR_SKEL 1 2 "A_Chase" enf-S_SKEL_RUN5)
    (enf--def-state states enf-S_SKEL_RUN5 "S_SKEL_RUN5" enf-SPR_SKEL 2 2 "A_Chase" enf-S_SKEL_RUN6)
    (enf--def-state states enf-S_SKEL_RUN6 "S_SKEL_RUN6" enf-SPR_SKEL 2 2 "A_Chase" enf-S_SKEL_RUN7)
    (enf--def-state states enf-S_SKEL_RUN7 "S_SKEL_RUN7" enf-SPR_SKEL 3 2 "A_Chase" enf-S_SKEL_RUN8)
    (enf--def-state states enf-S_SKEL_RUN8 "S_SKEL_RUN8" enf-SPR_SKEL 3 2 "A_Chase" enf-S_SKEL_RUN9)
    (enf--def-state states enf-S_SKEL_RUN9 "S_SKEL_RUN9" enf-SPR_SKEL 4 2 "A_Chase" enf-S_SKEL_RUN10)
    (enf--def-state states enf-S_SKEL_RUN10 "S_SKEL_RUN10" enf-SPR_SKEL 4 2 "A_Chase" enf-S_SKEL_RUN11)
    (enf--def-state states enf-S_SKEL_RUN11 "S_SKEL_RUN11" enf-SPR_SKEL 5 2 "A_Chase" enf-S_SKEL_RUN12)
    (enf--def-state states enf-S_SKEL_RUN12 "S_SKEL_RUN12" enf-SPR_SKEL 5 2 "A_Chase" enf-S_SKEL_RUN1)
    (enf--def-state states enf-S_SKEL_FIST1 "S_SKEL_FIST1" enf-SPR_SKEL 6 0 "A_FaceTarget" enf-S_SKEL_FIST2)
    (enf--def-state states enf-S_SKEL_FIST2 "S_SKEL_FIST2" enf-SPR_SKEL 6 6 "A_SkelWhoosh" enf-S_SKEL_FIST3)
    (enf--def-state states enf-S_SKEL_FIST3 "S_SKEL_FIST3" enf-SPR_SKEL 7 6 "A_FaceTarget" enf-S_SKEL_FIST4)
    (enf--def-state states enf-S_SKEL_FIST4 "S_SKEL_FIST4" enf-SPR_SKEL 8 6 "A_SkelFist" enf-S_SKEL_RUN1)
    (enf--def-state states enf-S_SKEL_MISS1 "S_SKEL_MISS1" enf-SPR_SKEL 32777 0 "A_FaceTarget" enf-S_SKEL_MISS2)
    (enf--def-state states enf-S_SKEL_MISS2 "S_SKEL_MISS2" enf-SPR_SKEL 32777 10 "A_FaceTarget" enf-S_SKEL_MISS3)
    (enf--def-state states enf-S_SKEL_MISS3 "S_SKEL_MISS3" enf-SPR_SKEL 10 10 "A_SkelMissile" enf-S_SKEL_MISS4)
    (enf--def-state states enf-S_SKEL_MISS4 "S_SKEL_MISS4" enf-SPR_SKEL 10 10 "A_FaceTarget" enf-S_SKEL_RUN1)
    (enf--def-state states enf-S_SKEL_PAIN "S_SKEL_PAIN" enf-SPR_SKEL 11 5 "NULL" enf-S_SKEL_PAIN2)
    (enf--def-state states enf-S_SKEL_PAIN2 "S_SKEL_PAIN2" enf-SPR_SKEL 11 5 "A_Pain" enf-S_SKEL_RUN1)
    (enf--def-state states enf-S_SKEL_DIE1 "S_SKEL_DIE1" enf-SPR_SKEL 11 7 "NULL" enf-S_SKEL_DIE2)
    (enf--def-state states enf-S_SKEL_DIE2 "S_SKEL_DIE2" enf-SPR_SKEL 12 7 "NULL" enf-S_SKEL_DIE3)
    (enf--def-state states enf-S_SKEL_DIE3 "S_SKEL_DIE3" enf-SPR_SKEL 13 7 "A_Scream" enf-S_SKEL_DIE4)
    (enf--def-state states enf-S_SKEL_DIE4 "S_SKEL_DIE4" enf-SPR_SKEL 14 7 "A_Fall" enf-S_SKEL_DIE5)
    (enf--def-state states enf-S_SKEL_DIE5 "S_SKEL_DIE5" enf-SPR_SKEL 15 7 "NULL" enf-S_SKEL_DIE6)
    (enf--def-state states enf-S_SKEL_DIE6 "S_SKEL_DIE6" enf-SPR_SKEL 16 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SKEL_RAISE1 "S_SKEL_RAISE1" enf-SPR_SKEL 16 5 "NULL" enf-S_SKEL_RAISE2)
    (enf--def-state states enf-S_SKEL_RAISE2 "S_SKEL_RAISE2" enf-SPR_SKEL 15 5 "NULL" enf-S_SKEL_RAISE3)
    (enf--def-state states enf-S_SKEL_RAISE3 "S_SKEL_RAISE3" enf-SPR_SKEL 14 5 "NULL" enf-S_SKEL_RAISE4)
    (enf--def-state states enf-S_SKEL_RAISE4 "S_SKEL_RAISE4" enf-SPR_SKEL 13 5 "NULL" enf-S_SKEL_RAISE5)
    (enf--def-state states enf-S_SKEL_RAISE5 "S_SKEL_RAISE5" enf-SPR_SKEL 12 5 "NULL" enf-S_SKEL_RAISE6)
    (enf--def-state states enf-S_SKEL_RAISE6 "S_SKEL_RAISE6" enf-SPR_SKEL 11 5 "NULL" enf-S_SKEL_RUN1)
    (enf--def-state states enf-S_FATSHOT1 "S_FATSHOT1" enf-SPR_MANF 32768 4 "NULL" enf-S_FATSHOT2)
    (enf--def-state states enf-S_FATSHOT2 "S_FATSHOT2" enf-SPR_MANF 32769 4 "NULL" enf-S_FATSHOT1)
    (enf--def-state states enf-S_FATSHOTX1 "S_FATSHOTX1" enf-SPR_MISL 32769 8 "NULL" enf-S_FATSHOTX2)
    (enf--def-state states enf-S_FATSHOTX2 "S_FATSHOTX2" enf-SPR_MISL 32770 6 "NULL" enf-S_FATSHOTX3)
    (enf--def-state states enf-S_FATSHOTX3 "S_FATSHOTX3" enf-SPR_MISL 32771 4 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_FATT_STND "S_FATT_STND" enf-SPR_FATT 0 15 "A_Look" enf-S_FATT_STND2)
    (enf--def-state states enf-S_FATT_STND2 "S_FATT_STND2" enf-SPR_FATT 1 15 "A_Look" enf-S_FATT_STND)
    (enf--def-state states enf-S_FATT_RUN1 "S_FATT_RUN1" enf-SPR_FATT 0 4 "A_Chase" enf-S_FATT_RUN2)
    (enf--def-state states enf-S_FATT_RUN2 "S_FATT_RUN2" enf-SPR_FATT 0 4 "A_Chase" enf-S_FATT_RUN3)
    (enf--def-state states enf-S_FATT_RUN3 "S_FATT_RUN3" enf-SPR_FATT 1 4 "A_Chase" enf-S_FATT_RUN4)
    (enf--def-state states enf-S_FATT_RUN4 "S_FATT_RUN4" enf-SPR_FATT 1 4 "A_Chase" enf-S_FATT_RUN5)
    (enf--def-state states enf-S_FATT_RUN5 "S_FATT_RUN5" enf-SPR_FATT 2 4 "A_Chase" enf-S_FATT_RUN6)
    (enf--def-state states enf-S_FATT_RUN6 "S_FATT_RUN6" enf-SPR_FATT 2 4 "A_Chase" enf-S_FATT_RUN7)
    (enf--def-state states enf-S_FATT_RUN7 "S_FATT_RUN7" enf-SPR_FATT 3 4 "A_Chase" enf-S_FATT_RUN8)
    (enf--def-state states enf-S_FATT_RUN8 "S_FATT_RUN8" enf-SPR_FATT 3 4 "A_Chase" enf-S_FATT_RUN9)
    (enf--def-state states enf-S_FATT_RUN9 "S_FATT_RUN9" enf-SPR_FATT 4 4 "A_Chase" enf-S_FATT_RUN10)
    (enf--def-state states enf-S_FATT_RUN10 "S_FATT_RUN10" enf-SPR_FATT 4 4 "A_Chase" enf-S_FATT_RUN11)
    (enf--def-state states enf-S_FATT_RUN11 "S_FATT_RUN11" enf-SPR_FATT 5 4 "A_Chase" enf-S_FATT_RUN12)
    (enf--def-state states enf-S_FATT_RUN12 "S_FATT_RUN12" enf-SPR_FATT 5 4 "A_Chase" enf-S_FATT_RUN1)
    (enf--def-state states enf-S_FATT_ATK1 "S_FATT_ATK1" enf-SPR_FATT 6 20 "A_FatRaise" enf-S_FATT_ATK2)
    (enf--def-state states enf-S_FATT_ATK2 "S_FATT_ATK2" enf-SPR_FATT 32775 10 "A_FatAttack1" enf-S_FATT_ATK3)
    (enf--def-state states enf-S_FATT_ATK3 "S_FATT_ATK3" enf-SPR_FATT 8 5 "A_FaceTarget" enf-S_FATT_ATK4)
    (enf--def-state states enf-S_FATT_ATK4 "S_FATT_ATK4" enf-SPR_FATT 6 5 "A_FaceTarget" enf-S_FATT_ATK5)
    (enf--def-state states enf-S_FATT_ATK5 "S_FATT_ATK5" enf-SPR_FATT 32775 10 "A_FatAttack2" enf-S_FATT_ATK6)
    (enf--def-state states enf-S_FATT_ATK6 "S_FATT_ATK6" enf-SPR_FATT 8 5 "A_FaceTarget" enf-S_FATT_ATK7)
    (enf--def-state states enf-S_FATT_ATK7 "S_FATT_ATK7" enf-SPR_FATT 6 5 "A_FaceTarget" enf-S_FATT_ATK8)
    (enf--def-state states enf-S_FATT_ATK8 "S_FATT_ATK8" enf-SPR_FATT 32775 10 "A_FatAttack3" enf-S_FATT_ATK9)
    (enf--def-state states enf-S_FATT_ATK9 "S_FATT_ATK9" enf-SPR_FATT 8 5 "A_FaceTarget" enf-S_FATT_ATK10)
    (enf--def-state states enf-S_FATT_ATK10 "S_FATT_ATK10" enf-SPR_FATT 6 5 "A_FaceTarget" enf-S_FATT_RUN1)
    (enf--def-state states enf-S_FATT_PAIN "S_FATT_PAIN" enf-SPR_FATT 9 3 "NULL" enf-S_FATT_PAIN2)
    (enf--def-state states enf-S_FATT_PAIN2 "S_FATT_PAIN2" enf-SPR_FATT 9 3 "A_Pain" enf-S_FATT_RUN1)
    (enf--def-state states enf-S_FATT_DIE1 "S_FATT_DIE1" enf-SPR_FATT 10 6 "NULL" enf-S_FATT_DIE2)
    (enf--def-state states enf-S_FATT_DIE2 "S_FATT_DIE2" enf-SPR_FATT 11 6 "A_Scream" enf-S_FATT_DIE3)
    (enf--def-state states enf-S_FATT_DIE3 "S_FATT_DIE3" enf-SPR_FATT 12 6 "A_Fall" enf-S_FATT_DIE4)
    (enf--def-state states enf-S_FATT_DIE4 "S_FATT_DIE4" enf-SPR_FATT 13 6 "NULL" enf-S_FATT_DIE5)
    (enf--def-state states enf-S_FATT_DIE5 "S_FATT_DIE5" enf-SPR_FATT 14 6 "NULL" enf-S_FATT_DIE6)
    (enf--def-state states enf-S_FATT_DIE6 "S_FATT_DIE6" enf-SPR_FATT 15 6 "NULL" enf-S_FATT_DIE7)
    (enf--def-state states enf-S_FATT_DIE7 "S_FATT_DIE7" enf-SPR_FATT 16 6 "NULL" enf-S_FATT_DIE8)
    (enf--def-state states enf-S_FATT_DIE8 "S_FATT_DIE8" enf-SPR_FATT 17 6 "NULL" enf-S_FATT_DIE9)
    (enf--def-state states enf-S_FATT_DIE9 "S_FATT_DIE9" enf-SPR_FATT 18 6 "NULL" enf-S_FATT_DIE10)
    (enf--def-state states enf-S_FATT_DIE10 "S_FATT_DIE10" enf-SPR_FATT 19 -1 "A_BossDeath" enf-S_NULL)
    (enf--def-state states enf-S_FATT_RAISE1 "S_FATT_RAISE1" enf-SPR_FATT 17 5 "NULL" enf-S_FATT_RAISE2)
    (enf--def-state states enf-S_FATT_RAISE2 "S_FATT_RAISE2" enf-SPR_FATT 16 5 "NULL" enf-S_FATT_RAISE3)
    (enf--def-state states enf-S_FATT_RAISE3 "S_FATT_RAISE3" enf-SPR_FATT 15 5 "NULL" enf-S_FATT_RAISE4)
    (enf--def-state states enf-S_FATT_RAISE4 "S_FATT_RAISE4" enf-SPR_FATT 14 5 "NULL" enf-S_FATT_RAISE5)
    (enf--def-state states enf-S_FATT_RAISE5 "S_FATT_RAISE5" enf-SPR_FATT 13 5 "NULL" enf-S_FATT_RAISE6)
    (enf--def-state states enf-S_FATT_RAISE6 "S_FATT_RAISE6" enf-SPR_FATT 12 5 "NULL" enf-S_FATT_RAISE7)
    (enf--def-state states enf-S_FATT_RAISE7 "S_FATT_RAISE7" enf-SPR_FATT 11 5 "NULL" enf-S_FATT_RAISE8)
    (enf--def-state states enf-S_FATT_RAISE8 "S_FATT_RAISE8" enf-SPR_FATT 10 5 "NULL" enf-S_FATT_RUN1)
    (enf--def-state states enf-S_CPOS_STND "S_CPOS_STND" enf-SPR_CPOS 0 10 "A_Look" enf-S_CPOS_STND2)
    (enf--def-state states enf-S_CPOS_STND2 "S_CPOS_STND2" enf-SPR_CPOS 1 10 "A_Look" enf-S_CPOS_STND)
    (enf--def-state states enf-S_CPOS_RUN1 "S_CPOS_RUN1" enf-SPR_CPOS 0 3 "A_Chase" enf-S_CPOS_RUN2)
    (enf--def-state states enf-S_CPOS_RUN2 "S_CPOS_RUN2" enf-SPR_CPOS 0 3 "A_Chase" enf-S_CPOS_RUN3)
    (enf--def-state states enf-S_CPOS_RUN3 "S_CPOS_RUN3" enf-SPR_CPOS 1 3 "A_Chase" enf-S_CPOS_RUN4)
    (enf--def-state states enf-S_CPOS_RUN4 "S_CPOS_RUN4" enf-SPR_CPOS 1 3 "A_Chase" enf-S_CPOS_RUN5)
    (enf--def-state states enf-S_CPOS_RUN5 "S_CPOS_RUN5" enf-SPR_CPOS 2 3 "A_Chase" enf-S_CPOS_RUN6)
    (enf--def-state states enf-S_CPOS_RUN6 "S_CPOS_RUN6" enf-SPR_CPOS 2 3 "A_Chase" enf-S_CPOS_RUN7)
    (enf--def-state states enf-S_CPOS_RUN7 "S_CPOS_RUN7" enf-SPR_CPOS 3 3 "A_Chase" enf-S_CPOS_RUN8)
    (enf--def-state states enf-S_CPOS_RUN8 "S_CPOS_RUN8" enf-SPR_CPOS 3 3 "A_Chase" enf-S_CPOS_RUN1)
    (enf--def-state states enf-S_CPOS_ATK1 "S_CPOS_ATK1" enf-SPR_CPOS 4 10 "A_FaceTarget" enf-S_CPOS_ATK2)
    (enf--def-state states enf-S_CPOS_ATK2 "S_CPOS_ATK2" enf-SPR_CPOS 32773 4 "A_CPosAttack" enf-S_CPOS_ATK3)
    (enf--def-state states enf-S_CPOS_ATK3 "S_CPOS_ATK3" enf-SPR_CPOS 32772 4 "A_CPosAttack" enf-S_CPOS_ATK4)
    (enf--def-state states enf-S_CPOS_ATK4 "S_CPOS_ATK4" enf-SPR_CPOS 5 1 "A_CPosRefire" enf-S_CPOS_ATK2)
    (enf--def-state states enf-S_CPOS_PAIN "S_CPOS_PAIN" enf-SPR_CPOS 6 3 "NULL" enf-S_CPOS_PAIN2)
    (enf--def-state states enf-S_CPOS_PAIN2 "S_CPOS_PAIN2" enf-SPR_CPOS 6 3 "A_Pain" enf-S_CPOS_RUN1)
    (enf--def-state states enf-S_CPOS_DIE1 "S_CPOS_DIE1" enf-SPR_CPOS 7 5 "NULL" enf-S_CPOS_DIE2)
    (enf--def-state states enf-S_CPOS_DIE2 "S_CPOS_DIE2" enf-SPR_CPOS 8 5 "A_Scream" enf-S_CPOS_DIE3)
    (enf--def-state states enf-S_CPOS_DIE3 "S_CPOS_DIE3" enf-SPR_CPOS 9 5 "A_Fall" enf-S_CPOS_DIE4)
    (enf--def-state states enf-S_CPOS_DIE4 "S_CPOS_DIE4" enf-SPR_CPOS 10 5 "NULL" enf-S_CPOS_DIE5)
    (enf--def-state states enf-S_CPOS_DIE5 "S_CPOS_DIE5" enf-SPR_CPOS 11 5 "NULL" enf-S_CPOS_DIE6)
    (enf--def-state states enf-S_CPOS_DIE6 "S_CPOS_DIE6" enf-SPR_CPOS 12 5 "NULL" enf-S_CPOS_DIE7)
    (enf--def-state states enf-S_CPOS_DIE7 "S_CPOS_DIE7" enf-SPR_CPOS 13 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CPOS_XDIE1 "S_CPOS_XDIE1" enf-SPR_CPOS 14 5 "NULL" enf-S_CPOS_XDIE2)
    (enf--def-state states enf-S_CPOS_XDIE2 "S_CPOS_XDIE2" enf-SPR_CPOS 15 5 "A_XScream" enf-S_CPOS_XDIE3)
    (enf--def-state states enf-S_CPOS_XDIE3 "S_CPOS_XDIE3" enf-SPR_CPOS 16 5 "A_Fall" enf-S_CPOS_XDIE4)
    (enf--def-state states enf-S_CPOS_XDIE4 "S_CPOS_XDIE4" enf-SPR_CPOS 17 5 "NULL" enf-S_CPOS_XDIE5)
    (enf--def-state states enf-S_CPOS_XDIE5 "S_CPOS_XDIE5" enf-SPR_CPOS 18 5 "NULL" enf-S_CPOS_XDIE6)
    (enf--def-state states enf-S_CPOS_XDIE6 "S_CPOS_XDIE6" enf-SPR_CPOS 19 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CPOS_RAISE1 "S_CPOS_RAISE1" enf-SPR_CPOS 13 5 "NULL" enf-S_CPOS_RAISE2)
    (enf--def-state states enf-S_CPOS_RAISE2 "S_CPOS_RAISE2" enf-SPR_CPOS 12 5 "NULL" enf-S_CPOS_RAISE3)
    (enf--def-state states enf-S_CPOS_RAISE3 "S_CPOS_RAISE3" enf-SPR_CPOS 11 5 "NULL" enf-S_CPOS_RAISE4)
    (enf--def-state states enf-S_CPOS_RAISE4 "S_CPOS_RAISE4" enf-SPR_CPOS 10 5 "NULL" enf-S_CPOS_RAISE5)
    (enf--def-state states enf-S_CPOS_RAISE5 "S_CPOS_RAISE5" enf-SPR_CPOS 9 5 "NULL" enf-S_CPOS_RAISE6)
    (enf--def-state states enf-S_CPOS_RAISE6 "S_CPOS_RAISE6" enf-SPR_CPOS 8 5 "NULL" enf-S_CPOS_RAISE7)
    (enf--def-state states enf-S_CPOS_RAISE7 "S_CPOS_RAISE7" enf-SPR_CPOS 7 5 "NULL" enf-S_CPOS_RUN1)
    (enf--def-state states enf-S_TROO_STND "S_TROO_STND" enf-SPR_TROO 0 10 "A_Look" enf-S_TROO_STND2)
    (enf--def-state states enf-S_TROO_STND2 "S_TROO_STND2" enf-SPR_TROO 1 10 "A_Look" enf-S_TROO_STND)
    (enf--def-state states enf-S_TROO_RUN1 "S_TROO_RUN1" enf-SPR_TROO 0 3 "A_Chase" enf-S_TROO_RUN2)
    (enf--def-state states enf-S_TROO_RUN2 "S_TROO_RUN2" enf-SPR_TROO 0 3 "A_Chase" enf-S_TROO_RUN3)
    (enf--def-state states enf-S_TROO_RUN3 "S_TROO_RUN3" enf-SPR_TROO 1 3 "A_Chase" enf-S_TROO_RUN4)
    (enf--def-state states enf-S_TROO_RUN4 "S_TROO_RUN4" enf-SPR_TROO 1 3 "A_Chase" enf-S_TROO_RUN5)
    (enf--def-state states enf-S_TROO_RUN5 "S_TROO_RUN5" enf-SPR_TROO 2 3 "A_Chase" enf-S_TROO_RUN6)
    (enf--def-state states enf-S_TROO_RUN6 "S_TROO_RUN6" enf-SPR_TROO 2 3 "A_Chase" enf-S_TROO_RUN7)
    (enf--def-state states enf-S_TROO_RUN7 "S_TROO_RUN7" enf-SPR_TROO 3 3 "A_Chase" enf-S_TROO_RUN8)
    (enf--def-state states enf-S_TROO_RUN8 "S_TROO_RUN8" enf-SPR_TROO 3 3 "A_Chase" enf-S_TROO_RUN1)
    (enf--def-state states enf-S_TROO_ATK1 "S_TROO_ATK1" enf-SPR_TROO 4 8 "A_FaceTarget" enf-S_TROO_ATK2)
    (enf--def-state states enf-S_TROO_ATK2 "S_TROO_ATK2" enf-SPR_TROO 5 8 "A_FaceTarget" enf-S_TROO_ATK3)
    (enf--def-state states enf-S_TROO_ATK3 "S_TROO_ATK3" enf-SPR_TROO 6 6 "A_TroopAttack" enf-S_TROO_RUN1)
    (enf--def-state states enf-S_TROO_PAIN "S_TROO_PAIN" enf-SPR_TROO 7 2 "NULL" enf-S_TROO_PAIN2)
    (enf--def-state states enf-S_TROO_PAIN2 "S_TROO_PAIN2" enf-SPR_TROO 7 2 "A_Pain" enf-S_TROO_RUN1)
    (enf--def-state states enf-S_TROO_DIE1 "S_TROO_DIE1" enf-SPR_TROO 8 8 "NULL" enf-S_TROO_DIE2)
    (enf--def-state states enf-S_TROO_DIE2 "S_TROO_DIE2" enf-SPR_TROO 9 8 "A_Scream" enf-S_TROO_DIE3)
    (enf--def-state states enf-S_TROO_DIE3 "S_TROO_DIE3" enf-SPR_TROO 10 6 "NULL" enf-S_TROO_DIE4)
    (enf--def-state states enf-S_TROO_DIE4 "S_TROO_DIE4" enf-SPR_TROO 11 6 "A_Fall" enf-S_TROO_DIE5)
    (enf--def-state states enf-S_TROO_DIE5 "S_TROO_DIE5" enf-SPR_TROO 12 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TROO_XDIE1 "S_TROO_XDIE1" enf-SPR_TROO 13 5 "NULL" enf-S_TROO_XDIE2)
    (enf--def-state states enf-S_TROO_XDIE2 "S_TROO_XDIE2" enf-SPR_TROO 14 5 "A_XScream" enf-S_TROO_XDIE3)
    (enf--def-state states enf-S_TROO_XDIE3 "S_TROO_XDIE3" enf-SPR_TROO 15 5 "NULL" enf-S_TROO_XDIE4)
    (enf--def-state states enf-S_TROO_XDIE4 "S_TROO_XDIE4" enf-SPR_TROO 16 5 "A_Fall" enf-S_TROO_XDIE5)
    (enf--def-state states enf-S_TROO_XDIE5 "S_TROO_XDIE5" enf-SPR_TROO 17 5 "NULL" enf-S_TROO_XDIE6)
    (enf--def-state states enf-S_TROO_XDIE6 "S_TROO_XDIE6" enf-SPR_TROO 18 5 "NULL" enf-S_TROO_XDIE7)
    (enf--def-state states enf-S_TROO_XDIE7 "S_TROO_XDIE7" enf-SPR_TROO 19 5 "NULL" enf-S_TROO_XDIE8)
    (enf--def-state states enf-S_TROO_XDIE8 "S_TROO_XDIE8" enf-SPR_TROO 20 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TROO_RAISE1 "S_TROO_RAISE1" enf-SPR_TROO 12 8 "NULL" enf-S_TROO_RAISE2)
    (enf--def-state states enf-S_TROO_RAISE2 "S_TROO_RAISE2" enf-SPR_TROO 11 8 "NULL" enf-S_TROO_RAISE3)
    (enf--def-state states enf-S_TROO_RAISE3 "S_TROO_RAISE3" enf-SPR_TROO 10 6 "NULL" enf-S_TROO_RAISE4)
    (enf--def-state states enf-S_TROO_RAISE4 "S_TROO_RAISE4" enf-SPR_TROO 9 6 "NULL" enf-S_TROO_RAISE5)
    (enf--def-state states enf-S_TROO_RAISE5 "S_TROO_RAISE5" enf-SPR_TROO 8 6 "NULL" enf-S_TROO_RUN1)
    (enf--def-state states enf-S_SARG_STND "S_SARG_STND" enf-SPR_SARG 0 10 "A_Look" enf-S_SARG_STND2)
    (enf--def-state states enf-S_SARG_STND2 "S_SARG_STND2" enf-SPR_SARG 1 10 "A_Look" enf-S_SARG_STND)
    (enf--def-state states enf-S_SARG_RUN1 "S_SARG_RUN1" enf-SPR_SARG 0 2 "A_Chase" enf-S_SARG_RUN2)
    (enf--def-state states enf-S_SARG_RUN2 "S_SARG_RUN2" enf-SPR_SARG 0 2 "A_Chase" enf-S_SARG_RUN3)
    (enf--def-state states enf-S_SARG_RUN3 "S_SARG_RUN3" enf-SPR_SARG 1 2 "A_Chase" enf-S_SARG_RUN4)
    (enf--def-state states enf-S_SARG_RUN4 "S_SARG_RUN4" enf-SPR_SARG 1 2 "A_Chase" enf-S_SARG_RUN5)
    (enf--def-state states enf-S_SARG_RUN5 "S_SARG_RUN5" enf-SPR_SARG 2 2 "A_Chase" enf-S_SARG_RUN6)
    (enf--def-state states enf-S_SARG_RUN6 "S_SARG_RUN6" enf-SPR_SARG 2 2 "A_Chase" enf-S_SARG_RUN7)
    (enf--def-state states enf-S_SARG_RUN7 "S_SARG_RUN7" enf-SPR_SARG 3 2 "A_Chase" enf-S_SARG_RUN8)
    (enf--def-state states enf-S_SARG_RUN8 "S_SARG_RUN8" enf-SPR_SARG 3 2 "A_Chase" enf-S_SARG_RUN1)
    (enf--def-state states enf-S_SARG_ATK1 "S_SARG_ATK1" enf-SPR_SARG 4 8 "A_FaceTarget" enf-S_SARG_ATK2)
    (enf--def-state states enf-S_SARG_ATK2 "S_SARG_ATK2" enf-SPR_SARG 5 8 "A_FaceTarget" enf-S_SARG_ATK3)
    (enf--def-state states enf-S_SARG_ATK3 "S_SARG_ATK3" enf-SPR_SARG 6 8 "A_SargAttack" enf-S_SARG_RUN1)
    (enf--def-state states enf-S_SARG_PAIN "S_SARG_PAIN" enf-SPR_SARG 7 2 "NULL" enf-S_SARG_PAIN2)
    (enf--def-state states enf-S_SARG_PAIN2 "S_SARG_PAIN2" enf-SPR_SARG 7 2 "A_Pain" enf-S_SARG_RUN1)
    (enf--def-state states enf-S_SARG_DIE1 "S_SARG_DIE1" enf-SPR_SARG 8 8 "NULL" enf-S_SARG_DIE2)
    (enf--def-state states enf-S_SARG_DIE2 "S_SARG_DIE2" enf-SPR_SARG 9 8 "A_Scream" enf-S_SARG_DIE3)
    (enf--def-state states enf-S_SARG_DIE3 "S_SARG_DIE3" enf-SPR_SARG 10 4 "NULL" enf-S_SARG_DIE4)
    (enf--def-state states enf-S_SARG_DIE4 "S_SARG_DIE4" enf-SPR_SARG 11 4 "A_Fall" enf-S_SARG_DIE5)
    (enf--def-state states enf-S_SARG_DIE5 "S_SARG_DIE5" enf-SPR_SARG 12 4 "NULL" enf-S_SARG_DIE6)
    (enf--def-state states enf-S_SARG_DIE6 "S_SARG_DIE6" enf-SPR_SARG 13 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SARG_RAISE1 "S_SARG_RAISE1" enf-SPR_SARG 13 5 "NULL" enf-S_SARG_RAISE2)
    (enf--def-state states enf-S_SARG_RAISE2 "S_SARG_RAISE2" enf-SPR_SARG 12 5 "NULL" enf-S_SARG_RAISE3)
    (enf--def-state states enf-S_SARG_RAISE3 "S_SARG_RAISE3" enf-SPR_SARG 11 5 "NULL" enf-S_SARG_RAISE4)
    (enf--def-state states enf-S_SARG_RAISE4 "S_SARG_RAISE4" enf-SPR_SARG 10 5 "NULL" enf-S_SARG_RAISE5)
    (enf--def-state states enf-S_SARG_RAISE5 "S_SARG_RAISE5" enf-SPR_SARG 9 5 "NULL" enf-S_SARG_RAISE6)
    (enf--def-state states enf-S_SARG_RAISE6 "S_SARG_RAISE6" enf-SPR_SARG 8 5 "NULL" enf-S_SARG_RUN1)
    (enf--def-state states enf-S_HEAD_STND "S_HEAD_STND" enf-SPR_HEAD 0 10 "A_Look" enf-S_HEAD_STND)
    (enf--def-state states enf-S_HEAD_RUN1 "S_HEAD_RUN1" enf-SPR_HEAD 0 3 "A_Chase" enf-S_HEAD_RUN1)
    (enf--def-state states enf-S_HEAD_ATK1 "S_HEAD_ATK1" enf-SPR_HEAD 1 5 "A_FaceTarget" enf-S_HEAD_ATK2)
    (enf--def-state states enf-S_HEAD_ATK2 "S_HEAD_ATK2" enf-SPR_HEAD 2 5 "A_FaceTarget" enf-S_HEAD_ATK3)
    (enf--def-state states enf-S_HEAD_ATK3 "S_HEAD_ATK3" enf-SPR_HEAD 32771 5 "A_HeadAttack" enf-S_HEAD_RUN1)
    (enf--def-state states enf-S_HEAD_PAIN "S_HEAD_PAIN" enf-SPR_HEAD 4 3 "NULL" enf-S_HEAD_PAIN2)
    (enf--def-state states enf-S_HEAD_PAIN2 "S_HEAD_PAIN2" enf-SPR_HEAD 4 3 "A_Pain" enf-S_HEAD_PAIN3)
    (enf--def-state states enf-S_HEAD_PAIN3 "S_HEAD_PAIN3" enf-SPR_HEAD 5 6 "NULL" enf-S_HEAD_RUN1)
    (enf--def-state states enf-S_HEAD_DIE1 "S_HEAD_DIE1" enf-SPR_HEAD 6 8 "NULL" enf-S_HEAD_DIE2)
    (enf--def-state states enf-S_HEAD_DIE2 "S_HEAD_DIE2" enf-SPR_HEAD 7 8 "A_Scream" enf-S_HEAD_DIE3)
    (enf--def-state states enf-S_HEAD_DIE3 "S_HEAD_DIE3" enf-SPR_HEAD 8 8 "NULL" enf-S_HEAD_DIE4)
    (enf--def-state states enf-S_HEAD_DIE4 "S_HEAD_DIE4" enf-SPR_HEAD 9 8 "NULL" enf-S_HEAD_DIE5)
    (enf--def-state states enf-S_HEAD_DIE5 "S_HEAD_DIE5" enf-SPR_HEAD 10 8 "A_Fall" enf-S_HEAD_DIE6)
    (enf--def-state states enf-S_HEAD_DIE6 "S_HEAD_DIE6" enf-SPR_HEAD 11 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HEAD_RAISE1 "S_HEAD_RAISE1" enf-SPR_HEAD 11 8 "NULL" enf-S_HEAD_RAISE2)
    (enf--def-state states enf-S_HEAD_RAISE2 "S_HEAD_RAISE2" enf-SPR_HEAD 10 8 "NULL" enf-S_HEAD_RAISE3)
    (enf--def-state states enf-S_HEAD_RAISE3 "S_HEAD_RAISE3" enf-SPR_HEAD 9 8 "NULL" enf-S_HEAD_RAISE4)
    (enf--def-state states enf-S_HEAD_RAISE4 "S_HEAD_RAISE4" enf-SPR_HEAD 8 8 "NULL" enf-S_HEAD_RAISE5)
    (enf--def-state states enf-S_HEAD_RAISE5 "S_HEAD_RAISE5" enf-SPR_HEAD 7 8 "NULL" enf-S_HEAD_RAISE6)
    (enf--def-state states enf-S_HEAD_RAISE6 "S_HEAD_RAISE6" enf-SPR_HEAD 6 8 "NULL" enf-S_HEAD_RUN1)
    (enf--def-state states enf-S_BRBALL1 "S_BRBALL1" enf-SPR_BAL7 32768 4 "NULL" enf-S_BRBALL2)
    (enf--def-state states enf-S_BRBALL2 "S_BRBALL2" enf-SPR_BAL7 32769 4 "NULL" enf-S_BRBALL1)
    (enf--def-state states enf-S_BRBALLX1 "S_BRBALLX1" enf-SPR_BAL7 32770 6 "NULL" enf-S_BRBALLX2)
    (enf--def-state states enf-S_BRBALLX2 "S_BRBALLX2" enf-SPR_BAL7 32771 6 "NULL" enf-S_BRBALLX3)
    (enf--def-state states enf-S_BRBALLX3 "S_BRBALLX3" enf-SPR_BAL7 32772 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BOSS_STND "S_BOSS_STND" enf-SPR_BOSS 0 10 "A_Look" enf-S_BOSS_STND2)
    (enf--def-state states enf-S_BOSS_STND2 "S_BOSS_STND2" enf-SPR_BOSS 1 10 "A_Look" enf-S_BOSS_STND)
    (enf--def-state states enf-S_BOSS_RUN1 "S_BOSS_RUN1" enf-SPR_BOSS 0 3 "A_Chase" enf-S_BOSS_RUN2)
    (enf--def-state states enf-S_BOSS_RUN2 "S_BOSS_RUN2" enf-SPR_BOSS 0 3 "A_Chase" enf-S_BOSS_RUN3)
    (enf--def-state states enf-S_BOSS_RUN3 "S_BOSS_RUN3" enf-SPR_BOSS 1 3 "A_Chase" enf-S_BOSS_RUN4)
    (enf--def-state states enf-S_BOSS_RUN4 "S_BOSS_RUN4" enf-SPR_BOSS 1 3 "A_Chase" enf-S_BOSS_RUN5)
    (enf--def-state states enf-S_BOSS_RUN5 "S_BOSS_RUN5" enf-SPR_BOSS 2 3 "A_Chase" enf-S_BOSS_RUN6)
    (enf--def-state states enf-S_BOSS_RUN6 "S_BOSS_RUN6" enf-SPR_BOSS 2 3 "A_Chase" enf-S_BOSS_RUN7)
    (enf--def-state states enf-S_BOSS_RUN7 "S_BOSS_RUN7" enf-SPR_BOSS 3 3 "A_Chase" enf-S_BOSS_RUN8)
    (enf--def-state states enf-S_BOSS_RUN8 "S_BOSS_RUN8" enf-SPR_BOSS 3 3 "A_Chase" enf-S_BOSS_RUN1)
    (enf--def-state states enf-S_BOSS_ATK1 "S_BOSS_ATK1" enf-SPR_BOSS 4 8 "A_FaceTarget" enf-S_BOSS_ATK2)
    (enf--def-state states enf-S_BOSS_ATK2 "S_BOSS_ATK2" enf-SPR_BOSS 5 8 "A_FaceTarget" enf-S_BOSS_ATK3)
    (enf--def-state states enf-S_BOSS_ATK3 "S_BOSS_ATK3" enf-SPR_BOSS 6 8 "A_BruisAttack" enf-S_BOSS_RUN1)
    (enf--def-state states enf-S_BOSS_PAIN "S_BOSS_PAIN" enf-SPR_BOSS 7 2 "NULL" enf-S_BOSS_PAIN2)
    (enf--def-state states enf-S_BOSS_PAIN2 "S_BOSS_PAIN2" enf-SPR_BOSS 7 2 "A_Pain" enf-S_BOSS_RUN1)
    (enf--def-state states enf-S_BOSS_DIE1 "S_BOSS_DIE1" enf-SPR_BOSS 8 8 "NULL" enf-S_BOSS_DIE2)
    (enf--def-state states enf-S_BOSS_DIE2 "S_BOSS_DIE2" enf-SPR_BOSS 9 8 "A_Scream" enf-S_BOSS_DIE3)
    (enf--def-state states enf-S_BOSS_DIE3 "S_BOSS_DIE3" enf-SPR_BOSS 10 8 "NULL" enf-S_BOSS_DIE4)
    (enf--def-state states enf-S_BOSS_DIE4 "S_BOSS_DIE4" enf-SPR_BOSS 11 8 "A_Fall" enf-S_BOSS_DIE5)
    (enf--def-state states enf-S_BOSS_DIE5 "S_BOSS_DIE5" enf-SPR_BOSS 12 8 "NULL" enf-S_BOSS_DIE6)
    (enf--def-state states enf-S_BOSS_DIE6 "S_BOSS_DIE6" enf-SPR_BOSS 13 8 "NULL" enf-S_BOSS_DIE7)
    (enf--def-state states enf-S_BOSS_DIE7 "S_BOSS_DIE7" enf-SPR_BOSS 14 -1 "A_BossDeath" enf-S_NULL)
    (enf--def-state states enf-S_BOSS_RAISE1 "S_BOSS_RAISE1" enf-SPR_BOSS 14 8 "NULL" enf-S_BOSS_RAISE2)
    (enf--def-state states enf-S_BOSS_RAISE2 "S_BOSS_RAISE2" enf-SPR_BOSS 13 8 "NULL" enf-S_BOSS_RAISE3)
    (enf--def-state states enf-S_BOSS_RAISE3 "S_BOSS_RAISE3" enf-SPR_BOSS 12 8 "NULL" enf-S_BOSS_RAISE4)
    (enf--def-state states enf-S_BOSS_RAISE4 "S_BOSS_RAISE4" enf-SPR_BOSS 11 8 "NULL" enf-S_BOSS_RAISE5)
    (enf--def-state states enf-S_BOSS_RAISE5 "S_BOSS_RAISE5" enf-SPR_BOSS 10 8 "NULL" enf-S_BOSS_RAISE6)
    (enf--def-state states enf-S_BOSS_RAISE6 "S_BOSS_RAISE6" enf-SPR_BOSS 9 8 "NULL" enf-S_BOSS_RAISE7)
    (enf--def-state states enf-S_BOSS_RAISE7 "S_BOSS_RAISE7" enf-SPR_BOSS 8 8 "NULL" enf-S_BOSS_RUN1)
    (enf--def-state states enf-S_BOS2_STND "S_BOS2_STND" enf-SPR_BOS2 0 10 "A_Look" enf-S_BOS2_STND2)
    (enf--def-state states enf-S_BOS2_STND2 "S_BOS2_STND2" enf-SPR_BOS2 1 10 "A_Look" enf-S_BOS2_STND)
    (enf--def-state states enf-S_BOS2_RUN1 "S_BOS2_RUN1" enf-SPR_BOS2 0 3 "A_Chase" enf-S_BOS2_RUN2)
    (enf--def-state states enf-S_BOS2_RUN2 "S_BOS2_RUN2" enf-SPR_BOS2 0 3 "A_Chase" enf-S_BOS2_RUN3)
    (enf--def-state states enf-S_BOS2_RUN3 "S_BOS2_RUN3" enf-SPR_BOS2 1 3 "A_Chase" enf-S_BOS2_RUN4)
    (enf--def-state states enf-S_BOS2_RUN4 "S_BOS2_RUN4" enf-SPR_BOS2 1 3 "A_Chase" enf-S_BOS2_RUN5)
    (enf--def-state states enf-S_BOS2_RUN5 "S_BOS2_RUN5" enf-SPR_BOS2 2 3 "A_Chase" enf-S_BOS2_RUN6)
    (enf--def-state states enf-S_BOS2_RUN6 "S_BOS2_RUN6" enf-SPR_BOS2 2 3 "A_Chase" enf-S_BOS2_RUN7)
    (enf--def-state states enf-S_BOS2_RUN7 "S_BOS2_RUN7" enf-SPR_BOS2 3 3 "A_Chase" enf-S_BOS2_RUN8)
    (enf--def-state states enf-S_BOS2_RUN8 "S_BOS2_RUN8" enf-SPR_BOS2 3 3 "A_Chase" enf-S_BOS2_RUN1)
    (enf--def-state states enf-S_BOS2_ATK1 "S_BOS2_ATK1" enf-SPR_BOS2 4 8 "A_FaceTarget" enf-S_BOS2_ATK2)
    (enf--def-state states enf-S_BOS2_ATK2 "S_BOS2_ATK2" enf-SPR_BOS2 5 8 "A_FaceTarget" enf-S_BOS2_ATK3)
    (enf--def-state states enf-S_BOS2_ATK3 "S_BOS2_ATK3" enf-SPR_BOS2 6 8 "A_BruisAttack" enf-S_BOS2_RUN1)
    (enf--def-state states enf-S_BOS2_PAIN "S_BOS2_PAIN" enf-SPR_BOS2 7 2 "NULL" enf-S_BOS2_PAIN2)
    (enf--def-state states enf-S_BOS2_PAIN2 "S_BOS2_PAIN2" enf-SPR_BOS2 7 2 "A_Pain" enf-S_BOS2_RUN1)
    (enf--def-state states enf-S_BOS2_DIE1 "S_BOS2_DIE1" enf-SPR_BOS2 8 8 "NULL" enf-S_BOS2_DIE2)
    (enf--def-state states enf-S_BOS2_DIE2 "S_BOS2_DIE2" enf-SPR_BOS2 9 8 "A_Scream" enf-S_BOS2_DIE3)
    (enf--def-state states enf-S_BOS2_DIE3 "S_BOS2_DIE3" enf-SPR_BOS2 10 8 "NULL" enf-S_BOS2_DIE4)
    (enf--def-state states enf-S_BOS2_DIE4 "S_BOS2_DIE4" enf-SPR_BOS2 11 8 "A_Fall" enf-S_BOS2_DIE5)
    (enf--def-state states enf-S_BOS2_DIE5 "S_BOS2_DIE5" enf-SPR_BOS2 12 8 "NULL" enf-S_BOS2_DIE6)
    (enf--def-state states enf-S_BOS2_DIE6 "S_BOS2_DIE6" enf-SPR_BOS2 13 8 "NULL" enf-S_BOS2_DIE7)
    (enf--def-state states enf-S_BOS2_DIE7 "S_BOS2_DIE7" enf-SPR_BOS2 14 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BOS2_RAISE1 "S_BOS2_RAISE1" enf-SPR_BOS2 14 8 "NULL" enf-S_BOS2_RAISE2)
    (enf--def-state states enf-S_BOS2_RAISE2 "S_BOS2_RAISE2" enf-SPR_BOS2 13 8 "NULL" enf-S_BOS2_RAISE3)
    (enf--def-state states enf-S_BOS2_RAISE3 "S_BOS2_RAISE3" enf-SPR_BOS2 12 8 "NULL" enf-S_BOS2_RAISE4)
    (enf--def-state states enf-S_BOS2_RAISE4 "S_BOS2_RAISE4" enf-SPR_BOS2 11 8 "NULL" enf-S_BOS2_RAISE5)
    (enf--def-state states enf-S_BOS2_RAISE5 "S_BOS2_RAISE5" enf-SPR_BOS2 10 8 "NULL" enf-S_BOS2_RAISE6)
    (enf--def-state states enf-S_BOS2_RAISE6 "S_BOS2_RAISE6" enf-SPR_BOS2 9 8 "NULL" enf-S_BOS2_RAISE7)
    (enf--def-state states enf-S_BOS2_RAISE7 "S_BOS2_RAISE7" enf-SPR_BOS2 8 8 "NULL" enf-S_BOS2_RUN1)
    (enf--def-state states enf-S_SKULL_STND "S_SKULL_STND" enf-SPR_SKUL 32768 10 "A_Look" enf-S_SKULL_STND2)
    (enf--def-state states enf-S_SKULL_STND2 "S_SKULL_STND2" enf-SPR_SKUL 32769 10 "A_Look" enf-S_SKULL_STND)
    (enf--def-state states enf-S_SKULL_RUN1 "S_SKULL_RUN1" enf-SPR_SKUL 32768 6 "A_Chase" enf-S_SKULL_RUN2)
    (enf--def-state states enf-S_SKULL_RUN2 "S_SKULL_RUN2" enf-SPR_SKUL 32769 6 "A_Chase" enf-S_SKULL_RUN1)
    (enf--def-state states enf-S_SKULL_ATK1 "S_SKULL_ATK1" enf-SPR_SKUL 32770 10 "A_FaceTarget" enf-S_SKULL_ATK2)
    (enf--def-state states enf-S_SKULL_ATK2 "S_SKULL_ATK2" enf-SPR_SKUL 32771 4 "A_SkullAttack" enf-S_SKULL_ATK3)
    (enf--def-state states enf-S_SKULL_ATK3 "S_SKULL_ATK3" enf-SPR_SKUL 32770 4 "NULL" enf-S_SKULL_ATK4)
    (enf--def-state states enf-S_SKULL_ATK4 "S_SKULL_ATK4" enf-SPR_SKUL 32771 4 "NULL" enf-S_SKULL_ATK3)
    (enf--def-state states enf-S_SKULL_PAIN "S_SKULL_PAIN" enf-SPR_SKUL 32772 3 "NULL" enf-S_SKULL_PAIN2)
    (enf--def-state states enf-S_SKULL_PAIN2 "S_SKULL_PAIN2" enf-SPR_SKUL 32772 3 "A_Pain" enf-S_SKULL_RUN1)
    (enf--def-state states enf-S_SKULL_DIE1 "S_SKULL_DIE1" enf-SPR_SKUL 32773 6 "NULL" enf-S_SKULL_DIE2)
    (enf--def-state states enf-S_SKULL_DIE2 "S_SKULL_DIE2" enf-SPR_SKUL 32774 6 "A_Scream" enf-S_SKULL_DIE3)
    (enf--def-state states enf-S_SKULL_DIE3 "S_SKULL_DIE3" enf-SPR_SKUL 32775 6 "NULL" enf-S_SKULL_DIE4)
    (enf--def-state states enf-S_SKULL_DIE4 "S_SKULL_DIE4" enf-SPR_SKUL 32776 6 "A_Fall" enf-S_SKULL_DIE5)
    (enf--def-state states enf-S_SKULL_DIE5 "S_SKULL_DIE5" enf-SPR_SKUL 9 6 "NULL" enf-S_SKULL_DIE6)
    (enf--def-state states enf-S_SKULL_DIE6 "S_SKULL_DIE6" enf-SPR_SKUL 10 6 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SPID_STND "S_SPID_STND" enf-SPR_SPID 0 10 "A_Look" enf-S_SPID_STND2)
    (enf--def-state states enf-S_SPID_STND2 "S_SPID_STND2" enf-SPR_SPID 1 10 "A_Look" enf-S_SPID_STND)
    (enf--def-state states enf-S_SPID_RUN1 "S_SPID_RUN1" enf-SPR_SPID 0 3 "A_Metal" enf-S_SPID_RUN2)
    (enf--def-state states enf-S_SPID_RUN2 "S_SPID_RUN2" enf-SPR_SPID 0 3 "A_Chase" enf-S_SPID_RUN3)
    (enf--def-state states enf-S_SPID_RUN3 "S_SPID_RUN3" enf-SPR_SPID 1 3 "A_Chase" enf-S_SPID_RUN4)
    (enf--def-state states enf-S_SPID_RUN4 "S_SPID_RUN4" enf-SPR_SPID 1 3 "A_Chase" enf-S_SPID_RUN5)
    (enf--def-state states enf-S_SPID_RUN5 "S_SPID_RUN5" enf-SPR_SPID 2 3 "A_Metal" enf-S_SPID_RUN6)
    (enf--def-state states enf-S_SPID_RUN6 "S_SPID_RUN6" enf-SPR_SPID 2 3 "A_Chase" enf-S_SPID_RUN7)
    (enf--def-state states enf-S_SPID_RUN7 "S_SPID_RUN7" enf-SPR_SPID 3 3 "A_Chase" enf-S_SPID_RUN8)
    (enf--def-state states enf-S_SPID_RUN8 "S_SPID_RUN8" enf-SPR_SPID 3 3 "A_Chase" enf-S_SPID_RUN9)
    (enf--def-state states enf-S_SPID_RUN9 "S_SPID_RUN9" enf-SPR_SPID 4 3 "A_Metal" enf-S_SPID_RUN10)
    (enf--def-state states enf-S_SPID_RUN10 "S_SPID_RUN10" enf-SPR_SPID 4 3 "A_Chase" enf-S_SPID_RUN11)
    (enf--def-state states enf-S_SPID_RUN11 "S_SPID_RUN11" enf-SPR_SPID 5 3 "A_Chase" enf-S_SPID_RUN12)
    (enf--def-state states enf-S_SPID_RUN12 "S_SPID_RUN12" enf-SPR_SPID 5 3 "A_Chase" enf-S_SPID_RUN1)
    (enf--def-state states enf-S_SPID_ATK1 "S_SPID_ATK1" enf-SPR_SPID 32768 20 "A_FaceTarget" enf-S_SPID_ATK2)
    (enf--def-state states enf-S_SPID_ATK2 "S_SPID_ATK2" enf-SPR_SPID 32774 4 "A_SPosAttack" enf-S_SPID_ATK3)
    (enf--def-state states enf-S_SPID_ATK3 "S_SPID_ATK3" enf-SPR_SPID 32775 4 "A_SPosAttack" enf-S_SPID_ATK4)
    (enf--def-state states enf-S_SPID_ATK4 "S_SPID_ATK4" enf-SPR_SPID 32775 1 "A_SpidRefire" enf-S_SPID_ATK2)
    (enf--def-state states enf-S_SPID_PAIN "S_SPID_PAIN" enf-SPR_SPID 8 3 "NULL" enf-S_SPID_PAIN2)
    (enf--def-state states enf-S_SPID_PAIN2 "S_SPID_PAIN2" enf-SPR_SPID 8 3 "A_Pain" enf-S_SPID_RUN1)
    (enf--def-state states enf-S_SPID_DIE1 "S_SPID_DIE1" enf-SPR_SPID 9 20 "A_Scream" enf-S_SPID_DIE2)
    (enf--def-state states enf-S_SPID_DIE2 "S_SPID_DIE2" enf-SPR_SPID 10 10 "A_Fall" enf-S_SPID_DIE3)
    (enf--def-state states enf-S_SPID_DIE3 "S_SPID_DIE3" enf-SPR_SPID 11 10 "NULL" enf-S_SPID_DIE4)
    (enf--def-state states enf-S_SPID_DIE4 "S_SPID_DIE4" enf-SPR_SPID 12 10 "NULL" enf-S_SPID_DIE5)
    (enf--def-state states enf-S_SPID_DIE5 "S_SPID_DIE5" enf-SPR_SPID 13 10 "NULL" enf-S_SPID_DIE6)
    (enf--def-state states enf-S_SPID_DIE6 "S_SPID_DIE6" enf-SPR_SPID 14 10 "NULL" enf-S_SPID_DIE7)
    (enf--def-state states enf-S_SPID_DIE7 "S_SPID_DIE7" enf-SPR_SPID 15 10 "NULL" enf-S_SPID_DIE8)
    (enf--def-state states enf-S_SPID_DIE8 "S_SPID_DIE8" enf-SPR_SPID 16 10 "NULL" enf-S_SPID_DIE9)
    (enf--def-state states enf-S_SPID_DIE9 "S_SPID_DIE9" enf-SPR_SPID 17 10 "NULL" enf-S_SPID_DIE10)
    (enf--def-state states enf-S_SPID_DIE10 "S_SPID_DIE10" enf-SPR_SPID 18 30 "NULL" enf-S_SPID_DIE11)
    (enf--def-state states enf-S_SPID_DIE11 "S_SPID_DIE11" enf-SPR_SPID 18 -1 "A_BossDeath" enf-S_NULL)
    (enf--def-state states enf-S_BSPI_STND "S_BSPI_STND" enf-SPR_BSPI 0 10 "A_Look" enf-S_BSPI_STND2)
    (enf--def-state states enf-S_BSPI_STND2 "S_BSPI_STND2" enf-SPR_BSPI 1 10 "A_Look" enf-S_BSPI_STND)
    (enf--def-state states enf-S_BSPI_SIGHT "S_BSPI_SIGHT" enf-SPR_BSPI 0 20 "NULL" enf-S_BSPI_RUN1)
    (enf--def-state states enf-S_BSPI_RUN1 "S_BSPI_RUN1" enf-SPR_BSPI 0 3 "A_BabyMetal" enf-S_BSPI_RUN2)
    (enf--def-state states enf-S_BSPI_RUN2 "S_BSPI_RUN2" enf-SPR_BSPI 0 3 "A_Chase" enf-S_BSPI_RUN3)
    (enf--def-state states enf-S_BSPI_RUN3 "S_BSPI_RUN3" enf-SPR_BSPI 1 3 "A_Chase" enf-S_BSPI_RUN4)
    (enf--def-state states enf-S_BSPI_RUN4 "S_BSPI_RUN4" enf-SPR_BSPI 1 3 "A_Chase" enf-S_BSPI_RUN5)
    (enf--def-state states enf-S_BSPI_RUN5 "S_BSPI_RUN5" enf-SPR_BSPI 2 3 "A_Chase" enf-S_BSPI_RUN6)
    (enf--def-state states enf-S_BSPI_RUN6 "S_BSPI_RUN6" enf-SPR_BSPI 2 3 "A_Chase" enf-S_BSPI_RUN7)
    (enf--def-state states enf-S_BSPI_RUN7 "S_BSPI_RUN7" enf-SPR_BSPI 3 3 "A_BabyMetal" enf-S_BSPI_RUN8)
    (enf--def-state states enf-S_BSPI_RUN8 "S_BSPI_RUN8" enf-SPR_BSPI 3 3 "A_Chase" enf-S_BSPI_RUN9)
    (enf--def-state states enf-S_BSPI_RUN9 "S_BSPI_RUN9" enf-SPR_BSPI 4 3 "A_Chase" enf-S_BSPI_RUN10)
    (enf--def-state states enf-S_BSPI_RUN10 "S_BSPI_RUN10" enf-SPR_BSPI 4 3 "A_Chase" enf-S_BSPI_RUN11)
    (enf--def-state states enf-S_BSPI_RUN11 "S_BSPI_RUN11" enf-SPR_BSPI 5 3 "A_Chase" enf-S_BSPI_RUN12)
    (enf--def-state states enf-S_BSPI_RUN12 "S_BSPI_RUN12" enf-SPR_BSPI 5 3 "A_Chase" enf-S_BSPI_RUN1)
    (enf--def-state states enf-S_BSPI_ATK1 "S_BSPI_ATK1" enf-SPR_BSPI 32768 20 "A_FaceTarget" enf-S_BSPI_ATK2)
    (enf--def-state states enf-S_BSPI_ATK2 "S_BSPI_ATK2" enf-SPR_BSPI 32774 4 "A_BspiAttack" enf-S_BSPI_ATK3)
    (enf--def-state states enf-S_BSPI_ATK3 "S_BSPI_ATK3" enf-SPR_BSPI 32775 4 "NULL" enf-S_BSPI_ATK4)
    (enf--def-state states enf-S_BSPI_ATK4 "S_BSPI_ATK4" enf-SPR_BSPI 32775 1 "A_SpidRefire" enf-S_BSPI_ATK2)
    (enf--def-state states enf-S_BSPI_PAIN "S_BSPI_PAIN" enf-SPR_BSPI 8 3 "NULL" enf-S_BSPI_PAIN2)
    (enf--def-state states enf-S_BSPI_PAIN2 "S_BSPI_PAIN2" enf-SPR_BSPI 8 3 "A_Pain" enf-S_BSPI_RUN1)
    (enf--def-state states enf-S_BSPI_DIE1 "S_BSPI_DIE1" enf-SPR_BSPI 9 20 "A_Scream" enf-S_BSPI_DIE2)
    (enf--def-state states enf-S_BSPI_DIE2 "S_BSPI_DIE2" enf-SPR_BSPI 10 7 "A_Fall" enf-S_BSPI_DIE3)
    (enf--def-state states enf-S_BSPI_DIE3 "S_BSPI_DIE3" enf-SPR_BSPI 11 7 "NULL" enf-S_BSPI_DIE4)
    (enf--def-state states enf-S_BSPI_DIE4 "S_BSPI_DIE4" enf-SPR_BSPI 12 7 "NULL" enf-S_BSPI_DIE5)
    (enf--def-state states enf-S_BSPI_DIE5 "S_BSPI_DIE5" enf-SPR_BSPI 13 7 "NULL" enf-S_BSPI_DIE6)
    (enf--def-state states enf-S_BSPI_DIE6 "S_BSPI_DIE6" enf-SPR_BSPI 14 7 "NULL" enf-S_BSPI_DIE7)
    (enf--def-state states enf-S_BSPI_DIE7 "S_BSPI_DIE7" enf-SPR_BSPI 15 -1 "A_BossDeath" enf-S_NULL)
    (enf--def-state states enf-S_BSPI_RAISE1 "S_BSPI_RAISE1" enf-SPR_BSPI 15 5 "NULL" enf-S_BSPI_RAISE2)
    (enf--def-state states enf-S_BSPI_RAISE2 "S_BSPI_RAISE2" enf-SPR_BSPI 14 5 "NULL" enf-S_BSPI_RAISE3)
    (enf--def-state states enf-S_BSPI_RAISE3 "S_BSPI_RAISE3" enf-SPR_BSPI 13 5 "NULL" enf-S_BSPI_RAISE4)
    (enf--def-state states enf-S_BSPI_RAISE4 "S_BSPI_RAISE4" enf-SPR_BSPI 12 5 "NULL" enf-S_BSPI_RAISE5)
    (enf--def-state states enf-S_BSPI_RAISE5 "S_BSPI_RAISE5" enf-SPR_BSPI 11 5 "NULL" enf-S_BSPI_RAISE6)
    (enf--def-state states enf-S_BSPI_RAISE6 "S_BSPI_RAISE6" enf-SPR_BSPI 10 5 "NULL" enf-S_BSPI_RAISE7)
    (enf--def-state states enf-S_BSPI_RAISE7 "S_BSPI_RAISE7" enf-SPR_BSPI 9 5 "NULL" enf-S_BSPI_RUN1)
    (enf--def-state states enf-S_ARACH_PLAZ "S_ARACH_PLAZ" enf-SPR_APLS 32768 5 "NULL" enf-S_ARACH_PLAZ2)
    (enf--def-state states enf-S_ARACH_PLAZ2 "S_ARACH_PLAZ2" enf-SPR_APLS 32769 5 "NULL" enf-S_ARACH_PLAZ)
    (enf--def-state states enf-S_ARACH_PLEX "S_ARACH_PLEX" enf-SPR_APBX 32768 5 "NULL" enf-S_ARACH_PLEX2)
    (enf--def-state states enf-S_ARACH_PLEX2 "S_ARACH_PLEX2" enf-SPR_APBX 32769 5 "NULL" enf-S_ARACH_PLEX3)
    (enf--def-state states enf-S_ARACH_PLEX3 "S_ARACH_PLEX3" enf-SPR_APBX 32770 5 "NULL" enf-S_ARACH_PLEX4)
    (enf--def-state states enf-S_ARACH_PLEX4 "S_ARACH_PLEX4" enf-SPR_APBX 32771 5 "NULL" enf-S_ARACH_PLEX5)
    (enf--def-state states enf-S_ARACH_PLEX5 "S_ARACH_PLEX5" enf-SPR_APBX 32772 5 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CYBER_STND "S_CYBER_STND" enf-SPR_CYBR 0 10 "A_Look" enf-S_CYBER_STND2)
    (enf--def-state states enf-S_CYBER_STND2 "S_CYBER_STND2" enf-SPR_CYBR 1 10 "A_Look" enf-S_CYBER_STND)
    (enf--def-state states enf-S_CYBER_RUN1 "S_CYBER_RUN1" enf-SPR_CYBR 0 3 "A_Hoof" enf-S_CYBER_RUN2)
    (enf--def-state states enf-S_CYBER_RUN2 "S_CYBER_RUN2" enf-SPR_CYBR 0 3 "A_Chase" enf-S_CYBER_RUN3)
    (enf--def-state states enf-S_CYBER_RUN3 "S_CYBER_RUN3" enf-SPR_CYBR 1 3 "A_Chase" enf-S_CYBER_RUN4)
    (enf--def-state states enf-S_CYBER_RUN4 "S_CYBER_RUN4" enf-SPR_CYBR 1 3 "A_Chase" enf-S_CYBER_RUN5)
    (enf--def-state states enf-S_CYBER_RUN5 "S_CYBER_RUN5" enf-SPR_CYBR 2 3 "A_Chase" enf-S_CYBER_RUN6)
    (enf--def-state states enf-S_CYBER_RUN6 "S_CYBER_RUN6" enf-SPR_CYBR 2 3 "A_Chase" enf-S_CYBER_RUN7)
    (enf--def-state states enf-S_CYBER_RUN7 "S_CYBER_RUN7" enf-SPR_CYBR 3 3 "A_Metal" enf-S_CYBER_RUN8)
    (enf--def-state states enf-S_CYBER_RUN8 "S_CYBER_RUN8" enf-SPR_CYBR 3 3 "A_Chase" enf-S_CYBER_RUN1)
    (enf--def-state states enf-S_CYBER_ATK1 "S_CYBER_ATK1" enf-SPR_CYBR 4 6 "A_FaceTarget" enf-S_CYBER_ATK2)
    (enf--def-state states enf-S_CYBER_ATK2 "S_CYBER_ATK2" enf-SPR_CYBR 5 12 "A_CyberAttack" enf-S_CYBER_ATK3)
    (enf--def-state states enf-S_CYBER_ATK3 "S_CYBER_ATK3" enf-SPR_CYBR 4 12 "A_FaceTarget" enf-S_CYBER_ATK4)
    (enf--def-state states enf-S_CYBER_ATK4 "S_CYBER_ATK4" enf-SPR_CYBR 5 12 "A_CyberAttack" enf-S_CYBER_ATK5)
    (enf--def-state states enf-S_CYBER_ATK5 "S_CYBER_ATK5" enf-SPR_CYBR 4 12 "A_FaceTarget" enf-S_CYBER_ATK6)
    (enf--def-state states enf-S_CYBER_ATK6 "S_CYBER_ATK6" enf-SPR_CYBR 5 12 "A_CyberAttack" enf-S_CYBER_RUN1)
    (enf--def-state states enf-S_CYBER_PAIN "S_CYBER_PAIN" enf-SPR_CYBR 6 10 "A_Pain" enf-S_CYBER_RUN1)
    (enf--def-state states enf-S_CYBER_DIE1 "S_CYBER_DIE1" enf-SPR_CYBR 7 10 "NULL" enf-S_CYBER_DIE2)
    (enf--def-state states enf-S_CYBER_DIE2 "S_CYBER_DIE2" enf-SPR_CYBR 8 10 "A_Scream" enf-S_CYBER_DIE3)
    (enf--def-state states enf-S_CYBER_DIE3 "S_CYBER_DIE3" enf-SPR_CYBR 9 10 "NULL" enf-S_CYBER_DIE4)
    (enf--def-state states enf-S_CYBER_DIE4 "S_CYBER_DIE4" enf-SPR_CYBR 10 10 "NULL" enf-S_CYBER_DIE5)
    (enf--def-state states enf-S_CYBER_DIE5 "S_CYBER_DIE5" enf-SPR_CYBR 11 10 "NULL" enf-S_CYBER_DIE6)
    (enf--def-state states enf-S_CYBER_DIE6 "S_CYBER_DIE6" enf-SPR_CYBR 12 10 "A_Fall" enf-S_CYBER_DIE7)
    (enf--def-state states enf-S_CYBER_DIE7 "S_CYBER_DIE7" enf-SPR_CYBR 13 10 "NULL" enf-S_CYBER_DIE8)
    (enf--def-state states enf-S_CYBER_DIE8 "S_CYBER_DIE8" enf-SPR_CYBR 14 10 "NULL" enf-S_CYBER_DIE9)
    (enf--def-state states enf-S_CYBER_DIE9 "S_CYBER_DIE9" enf-SPR_CYBR 15 30 "NULL" enf-S_CYBER_DIE10)
    (enf--def-state states enf-S_CYBER_DIE10 "S_CYBER_DIE10" enf-SPR_CYBR 15 -1 "A_BossDeath" enf-S_NULL)
    (enf--def-state states enf-S_PAIN_STND "S_PAIN_STND" enf-SPR_PAIN 0 10 "A_Look" enf-S_PAIN_STND)
    (enf--def-state states enf-S_PAIN_RUN1 "S_PAIN_RUN1" enf-SPR_PAIN 0 3 "A_Chase" enf-S_PAIN_RUN2)
    (enf--def-state states enf-S_PAIN_RUN2 "S_PAIN_RUN2" enf-SPR_PAIN 0 3 "A_Chase" enf-S_PAIN_RUN3)
    (enf--def-state states enf-S_PAIN_RUN3 "S_PAIN_RUN3" enf-SPR_PAIN 1 3 "A_Chase" enf-S_PAIN_RUN4)
    (enf--def-state states enf-S_PAIN_RUN4 "S_PAIN_RUN4" enf-SPR_PAIN 1 3 "A_Chase" enf-S_PAIN_RUN5)
    (enf--def-state states enf-S_PAIN_RUN5 "S_PAIN_RUN5" enf-SPR_PAIN 2 3 "A_Chase" enf-S_PAIN_RUN6)
    (enf--def-state states enf-S_PAIN_RUN6 "S_PAIN_RUN6" enf-SPR_PAIN 2 3 "A_Chase" enf-S_PAIN_RUN1)
    (enf--def-state states enf-S_PAIN_ATK1 "S_PAIN_ATK1" enf-SPR_PAIN 3 5 "A_FaceTarget" enf-S_PAIN_ATK2)
    (enf--def-state states enf-S_PAIN_ATK2 "S_PAIN_ATK2" enf-SPR_PAIN 4 5 "A_FaceTarget" enf-S_PAIN_ATK3)
    (enf--def-state states enf-S_PAIN_ATK3 "S_PAIN_ATK3" enf-SPR_PAIN 32773 5 "A_FaceTarget" enf-S_PAIN_ATK4)
    (enf--def-state states enf-S_PAIN_ATK4 "S_PAIN_ATK4" enf-SPR_PAIN 32773 0 "A_PainAttack" enf-S_PAIN_RUN1)
    (enf--def-state states enf-S_PAIN_PAIN "S_PAIN_PAIN" enf-SPR_PAIN 6 6 "NULL" enf-S_PAIN_PAIN2)
    (enf--def-state states enf-S_PAIN_PAIN2 "S_PAIN_PAIN2" enf-SPR_PAIN 6 6 "A_Pain" enf-S_PAIN_RUN1)
    (enf--def-state states enf-S_PAIN_DIE1 "S_PAIN_DIE1" enf-SPR_PAIN 32775 8 "NULL" enf-S_PAIN_DIE2)
    (enf--def-state states enf-S_PAIN_DIE2 "S_PAIN_DIE2" enf-SPR_PAIN 32776 8 "A_Scream" enf-S_PAIN_DIE3)
    (enf--def-state states enf-S_PAIN_DIE3 "S_PAIN_DIE3" enf-SPR_PAIN 32777 8 "NULL" enf-S_PAIN_DIE4)
    (enf--def-state states enf-S_PAIN_DIE4 "S_PAIN_DIE4" enf-SPR_PAIN 32778 8 "NULL" enf-S_PAIN_DIE5)
    (enf--def-state states enf-S_PAIN_DIE5 "S_PAIN_DIE5" enf-SPR_PAIN 32779 8 "A_PainDie" enf-S_PAIN_DIE6)
    (enf--def-state states enf-S_PAIN_DIE6 "S_PAIN_DIE6" enf-SPR_PAIN 32780 8 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PAIN_RAISE1 "S_PAIN_RAISE1" enf-SPR_PAIN 12 8 "NULL" enf-S_PAIN_RAISE2)
    (enf--def-state states enf-S_PAIN_RAISE2 "S_PAIN_RAISE2" enf-SPR_PAIN 11 8 "NULL" enf-S_PAIN_RAISE3)
    (enf--def-state states enf-S_PAIN_RAISE3 "S_PAIN_RAISE3" enf-SPR_PAIN 10 8 "NULL" enf-S_PAIN_RAISE4)
    (enf--def-state states enf-S_PAIN_RAISE4 "S_PAIN_RAISE4" enf-SPR_PAIN 9 8 "NULL" enf-S_PAIN_RAISE5)
    (enf--def-state states enf-S_PAIN_RAISE5 "S_PAIN_RAISE5" enf-SPR_PAIN 8 8 "NULL" enf-S_PAIN_RAISE6)
    (enf--def-state states enf-S_PAIN_RAISE6 "S_PAIN_RAISE6" enf-SPR_PAIN 7 8 "NULL" enf-S_PAIN_RUN1)
    (enf--def-state states enf-S_SSWV_STND "S_SSWV_STND" enf-SPR_SSWV 0 10 "A_Look" enf-S_SSWV_STND2)
    (enf--def-state states enf-S_SSWV_STND2 "S_SSWV_STND2" enf-SPR_SSWV 1 10 "A_Look" enf-S_SSWV_STND)
    (enf--def-state states enf-S_SSWV_RUN1 "S_SSWV_RUN1" enf-SPR_SSWV 0 3 "A_Chase" enf-S_SSWV_RUN2)
    (enf--def-state states enf-S_SSWV_RUN2 "S_SSWV_RUN2" enf-SPR_SSWV 0 3 "A_Chase" enf-S_SSWV_RUN3)
    (enf--def-state states enf-S_SSWV_RUN3 "S_SSWV_RUN3" enf-SPR_SSWV 1 3 "A_Chase" enf-S_SSWV_RUN4)
    (enf--def-state states enf-S_SSWV_RUN4 "S_SSWV_RUN4" enf-SPR_SSWV 1 3 "A_Chase" enf-S_SSWV_RUN5)
    (enf--def-state states enf-S_SSWV_RUN5 "S_SSWV_RUN5" enf-SPR_SSWV 2 3 "A_Chase" enf-S_SSWV_RUN6)
    (enf--def-state states enf-S_SSWV_RUN6 "S_SSWV_RUN6" enf-SPR_SSWV 2 3 "A_Chase" enf-S_SSWV_RUN7)
    (enf--def-state states enf-S_SSWV_RUN7 "S_SSWV_RUN7" enf-SPR_SSWV 3 3 "A_Chase" enf-S_SSWV_RUN8)
    (enf--def-state states enf-S_SSWV_RUN8 "S_SSWV_RUN8" enf-SPR_SSWV 3 3 "A_Chase" enf-S_SSWV_RUN1)
    (enf--def-state states enf-S_SSWV_ATK1 "S_SSWV_ATK1" enf-SPR_SSWV 4 10 "A_FaceTarget" enf-S_SSWV_ATK2)
    (enf--def-state states enf-S_SSWV_ATK2 "S_SSWV_ATK2" enf-SPR_SSWV 5 10 "A_FaceTarget" enf-S_SSWV_ATK3)
    (enf--def-state states enf-S_SSWV_ATK3 "S_SSWV_ATK3" enf-SPR_SSWV 32774 4 "A_CPosAttack" enf-S_SSWV_ATK4)
    (enf--def-state states enf-S_SSWV_ATK4 "S_SSWV_ATK4" enf-SPR_SSWV 5 6 "A_FaceTarget" enf-S_SSWV_ATK5)
    (enf--def-state states enf-S_SSWV_ATK5 "S_SSWV_ATK5" enf-SPR_SSWV 32774 4 "A_CPosAttack" enf-S_SSWV_ATK6)
    (enf--def-state states enf-S_SSWV_ATK6 "S_SSWV_ATK6" enf-SPR_SSWV 5 1 "A_CPosRefire" enf-S_SSWV_ATK2)
    (enf--def-state states enf-S_SSWV_PAIN "S_SSWV_PAIN" enf-SPR_SSWV 7 3 "NULL" enf-S_SSWV_PAIN2)
    (enf--def-state states enf-S_SSWV_PAIN2 "S_SSWV_PAIN2" enf-SPR_SSWV 7 3 "A_Pain" enf-S_SSWV_RUN1)
    (enf--def-state states enf-S_SSWV_DIE1 "S_SSWV_DIE1" enf-SPR_SSWV 8 5 "NULL" enf-S_SSWV_DIE2)
    (enf--def-state states enf-S_SSWV_DIE2 "S_SSWV_DIE2" enf-SPR_SSWV 9 5 "A_Scream" enf-S_SSWV_DIE3)
    (enf--def-state states enf-S_SSWV_DIE3 "S_SSWV_DIE3" enf-SPR_SSWV 10 5 "A_Fall" enf-S_SSWV_DIE4)
    (enf--def-state states enf-S_SSWV_DIE4 "S_SSWV_DIE4" enf-SPR_SSWV 11 5 "NULL" enf-S_SSWV_DIE5)
    (enf--def-state states enf-S_SSWV_DIE5 "S_SSWV_DIE5" enf-SPR_SSWV 12 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SSWV_XDIE1 "S_SSWV_XDIE1" enf-SPR_SSWV 13 5 "NULL" enf-S_SSWV_XDIE2)
    (enf--def-state states enf-S_SSWV_XDIE2 "S_SSWV_XDIE2" enf-SPR_SSWV 14 5 "A_XScream" enf-S_SSWV_XDIE3)
    (enf--def-state states enf-S_SSWV_XDIE3 "S_SSWV_XDIE3" enf-SPR_SSWV 15 5 "A_Fall" enf-S_SSWV_XDIE4)
    (enf--def-state states enf-S_SSWV_XDIE4 "S_SSWV_XDIE4" enf-SPR_SSWV 16 5 "NULL" enf-S_SSWV_XDIE5)
    (enf--def-state states enf-S_SSWV_XDIE5 "S_SSWV_XDIE5" enf-SPR_SSWV 17 5 "NULL" enf-S_SSWV_XDIE6)
    (enf--def-state states enf-S_SSWV_XDIE6 "S_SSWV_XDIE6" enf-SPR_SSWV 18 5 "NULL" enf-S_SSWV_XDIE7)
    (enf--def-state states enf-S_SSWV_XDIE7 "S_SSWV_XDIE7" enf-SPR_SSWV 19 5 "NULL" enf-S_SSWV_XDIE8)
    (enf--def-state states enf-S_SSWV_XDIE8 "S_SSWV_XDIE8" enf-SPR_SSWV 20 5 "NULL" enf-S_SSWV_XDIE9)
    (enf--def-state states enf-S_SSWV_XDIE9 "S_SSWV_XDIE9" enf-SPR_SSWV 21 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SSWV_RAISE1 "S_SSWV_RAISE1" enf-SPR_SSWV 12 5 "NULL" enf-S_SSWV_RAISE2)
    (enf--def-state states enf-S_SSWV_RAISE2 "S_SSWV_RAISE2" enf-SPR_SSWV 11 5 "NULL" enf-S_SSWV_RAISE3)
    (enf--def-state states enf-S_SSWV_RAISE3 "S_SSWV_RAISE3" enf-SPR_SSWV 10 5 "NULL" enf-S_SSWV_RAISE4)
    (enf--def-state states enf-S_SSWV_RAISE4 "S_SSWV_RAISE4" enf-SPR_SSWV 9 5 "NULL" enf-S_SSWV_RAISE5)
    (enf--def-state states enf-S_SSWV_RAISE5 "S_SSWV_RAISE5" enf-SPR_SSWV 8 5 "NULL" enf-S_SSWV_RUN1)
    (enf--def-state states enf-S_KEENSTND "S_KEENSTND" enf-SPR_KEEN 0 -1 "NULL" enf-S_KEENSTND)
    (enf--def-state states enf-S_COMMKEEN "S_COMMKEEN" enf-SPR_KEEN 0 6 "NULL" enf-S_COMMKEEN2)
    (enf--def-state states enf-S_COMMKEEN2 "S_COMMKEEN2" enf-SPR_KEEN 1 6 "NULL" enf-S_COMMKEEN3)
    (enf--def-state states enf-S_COMMKEEN3 "S_COMMKEEN3" enf-SPR_KEEN 2 6 "A_Scream" enf-S_COMMKEEN4)
    (enf--def-state states enf-S_COMMKEEN4 "S_COMMKEEN4" enf-SPR_KEEN 3 6 "NULL" enf-S_COMMKEEN5)
    (enf--def-state states enf-S_COMMKEEN5 "S_COMMKEEN5" enf-SPR_KEEN 4 6 "NULL" enf-S_COMMKEEN6)
    (enf--def-state states enf-S_COMMKEEN6 "S_COMMKEEN6" enf-SPR_KEEN 5 6 "NULL" enf-S_COMMKEEN7)
    (enf--def-state states enf-S_COMMKEEN7 "S_COMMKEEN7" enf-SPR_KEEN 6 6 "NULL" enf-S_COMMKEEN8)
    (enf--def-state states enf-S_COMMKEEN8 "S_COMMKEEN8" enf-SPR_KEEN 7 6 "NULL" enf-S_COMMKEEN9)
    (enf--def-state states enf-S_COMMKEEN9 "S_COMMKEEN9" enf-SPR_KEEN 8 6 "NULL" enf-S_COMMKEEN10)
    (enf--def-state states enf-S_COMMKEEN10 "S_COMMKEEN10" enf-SPR_KEEN 9 6 "NULL" enf-S_COMMKEEN11)
    (enf--def-state states enf-S_COMMKEEN11 "S_COMMKEEN11" enf-SPR_KEEN 10 6 "A_KeenDie" enf-S_COMMKEEN12)
    (enf--def-state states enf-S_COMMKEEN12 "S_COMMKEEN12" enf-SPR_KEEN 11 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_KEENPAIN "S_KEENPAIN" enf-SPR_KEEN 12 4 "NULL" enf-S_KEENPAIN2)
    (enf--def-state states enf-S_KEENPAIN2 "S_KEENPAIN2" enf-SPR_KEEN 12 8 "A_Pain" enf-S_KEENSTND)
    (enf--def-state states enf-S_BRAIN "S_BRAIN" enf-SPR_BBRN 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BRAIN_PAIN "S_BRAIN_PAIN" enf-SPR_BBRN 1 36 "A_BrainPain" enf-S_BRAIN)
    (enf--def-state states enf-S_BRAIN_DIE1 "S_BRAIN_DIE1" enf-SPR_BBRN 0 100 "A_BrainScream" enf-S_BRAIN_DIE2)
    (enf--def-state states enf-S_BRAIN_DIE2 "S_BRAIN_DIE2" enf-SPR_BBRN 0 10 "NULL" enf-S_BRAIN_DIE3)
    (enf--def-state states enf-S_BRAIN_DIE3 "S_BRAIN_DIE3" enf-SPR_BBRN 0 10 "NULL" enf-S_BRAIN_DIE4)
    (enf--def-state states enf-S_BRAIN_DIE4 "S_BRAIN_DIE4" enf-SPR_BBRN 0 -1 "A_BrainDie" enf-S_NULL)
    (enf--def-state states enf-S_BRAINEYE "S_BRAINEYE" enf-SPR_SSWV 0 10 "A_Look" enf-S_BRAINEYE)
    (enf--def-state states enf-S_BRAINEYESEE "S_BRAINEYESEE" enf-SPR_SSWV 0 181 "A_BrainAwake" enf-S_BRAINEYE1)
    (enf--def-state states enf-S_BRAINEYE1 "S_BRAINEYE1" enf-SPR_SSWV 0 150 "A_BrainSpit" enf-S_BRAINEYE1)
    (enf--def-state states enf-S_SPAWN1 "S_SPAWN1" enf-SPR_BOSF 32768 3 "A_SpawnSound" enf-S_SPAWN2)
    (enf--def-state states enf-S_SPAWN2 "S_SPAWN2" enf-SPR_BOSF 32769 3 "A_SpawnFly" enf-S_SPAWN3)
    (enf--def-state states enf-S_SPAWN3 "S_SPAWN3" enf-SPR_BOSF 32770 3 "A_SpawnFly" enf-S_SPAWN4)
    (enf--def-state states enf-S_SPAWN4 "S_SPAWN4" enf-SPR_BOSF 32771 3 "A_SpawnFly" enf-S_SPAWN1)
    (enf--def-state states enf-S_SPAWNFIRE1 "S_SPAWNFIRE1" enf-SPR_FIRE 32768 4 "A_Fire" enf-S_SPAWNFIRE2)
    (enf--def-state states enf-S_SPAWNFIRE2 "S_SPAWNFIRE2" enf-SPR_FIRE 32769 4 "A_Fire" enf-S_SPAWNFIRE3)
    (enf--def-state states enf-S_SPAWNFIRE3 "S_SPAWNFIRE3" enf-SPR_FIRE 32770 4 "A_Fire" enf-S_SPAWNFIRE4)
    (enf--def-state states enf-S_SPAWNFIRE4 "S_SPAWNFIRE4" enf-SPR_FIRE 32771 4 "A_Fire" enf-S_SPAWNFIRE5)
    (enf--def-state states enf-S_SPAWNFIRE5 "S_SPAWNFIRE5" enf-SPR_FIRE 32772 4 "A_Fire" enf-S_SPAWNFIRE6)
    (enf--def-state states enf-S_SPAWNFIRE6 "S_SPAWNFIRE6" enf-SPR_FIRE 32773 4 "A_Fire" enf-S_SPAWNFIRE7)
    (enf--def-state states enf-S_SPAWNFIRE7 "S_SPAWNFIRE7" enf-SPR_FIRE 32774 4 "A_Fire" enf-S_SPAWNFIRE8)
    (enf--def-state states enf-S_SPAWNFIRE8 "S_SPAWNFIRE8" enf-SPR_FIRE 32775 4 "A_Fire" enf-S_NULL)
    (enf--def-state states enf-S_BRAINEXPLODE1 "S_BRAINEXPLODE1" enf-SPR_MISL 32769 10 "NULL" enf-S_BRAINEXPLODE2)
    (enf--def-state states enf-S_BRAINEXPLODE2 "S_BRAINEXPLODE2" enf-SPR_MISL 32770 10 "NULL" enf-S_BRAINEXPLODE3)
    (enf--def-state states enf-S_BRAINEXPLODE3 "S_BRAINEXPLODE3" enf-SPR_MISL 32771 10 "A_BrainExplode" enf-S_NULL)
    (enf--def-state states enf-S_ARM1 "S_ARM1" enf-SPR_ARM1 0 6 "NULL" enf-S_ARM1A)
    (enf--def-state states enf-S_ARM1A "S_ARM1A" enf-SPR_ARM1 32769 7 "NULL" enf-S_ARM1)
    (enf--def-state states enf-S_ARM2 "S_ARM2" enf-SPR_ARM2 0 6 "NULL" enf-S_ARM2A)
    (enf--def-state states enf-S_ARM2A "S_ARM2A" enf-SPR_ARM2 32769 6 "NULL" enf-S_ARM2)
    (enf--def-state states enf-S_BAR1 "S_BAR1" enf-SPR_BAR1 0 6 "NULL" enf-S_BAR2)
    (enf--def-state states enf-S_BAR2 "S_BAR2" enf-SPR_BAR1 1 6 "NULL" enf-S_BAR1)
    (enf--def-state states enf-S_BEXP "S_BEXP" enf-SPR_BEXP 32768 5 "NULL" enf-S_BEXP2)
    (enf--def-state states enf-S_BEXP2 "S_BEXP2" enf-SPR_BEXP 32769 5 "A_Scream" enf-S_BEXP3)
    (enf--def-state states enf-S_BEXP3 "S_BEXP3" enf-SPR_BEXP 32770 5 "NULL" enf-S_BEXP4)
    (enf--def-state states enf-S_BEXP4 "S_BEXP4" enf-SPR_BEXP 32771 10 "A_Explode" enf-S_BEXP5)
    (enf--def-state states enf-S_BEXP5 "S_BEXP5" enf-SPR_BEXP 32772 10 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BBAR1 "S_BBAR1" enf-SPR_FCAN 32768 4 "NULL" enf-S_BBAR2)
    (enf--def-state states enf-S_BBAR2 "S_BBAR2" enf-SPR_FCAN 32769 4 "NULL" enf-S_BBAR3)
    (enf--def-state states enf-S_BBAR3 "S_BBAR3" enf-SPR_FCAN 32770 4 "NULL" enf-S_BBAR1)
    (enf--def-state states enf-S_BON1 "S_BON1" enf-SPR_BON1 0 6 "NULL" enf-S_BON1A)
    (enf--def-state states enf-S_BON1A "S_BON1A" enf-SPR_BON1 1 6 "NULL" enf-S_BON1B)
    (enf--def-state states enf-S_BON1B "S_BON1B" enf-SPR_BON1 2 6 "NULL" enf-S_BON1C)
    (enf--def-state states enf-S_BON1C "S_BON1C" enf-SPR_BON1 3 6 "NULL" enf-S_BON1D)
    (enf--def-state states enf-S_BON1D "S_BON1D" enf-SPR_BON1 2 6 "NULL" enf-S_BON1E)
    (enf--def-state states enf-S_BON1E "S_BON1E" enf-SPR_BON1 1 6 "NULL" enf-S_BON1)
    (enf--def-state states enf-S_BON2 "S_BON2" enf-SPR_BON2 0 6 "NULL" enf-S_BON2A)
    (enf--def-state states enf-S_BON2A "S_BON2A" enf-SPR_BON2 1 6 "NULL" enf-S_BON2B)
    (enf--def-state states enf-S_BON2B "S_BON2B" enf-SPR_BON2 2 6 "NULL" enf-S_BON2C)
    (enf--def-state states enf-S_BON2C "S_BON2C" enf-SPR_BON2 3 6 "NULL" enf-S_BON2D)
    (enf--def-state states enf-S_BON2D "S_BON2D" enf-SPR_BON2 2 6 "NULL" enf-S_BON2E)
    (enf--def-state states enf-S_BON2E "S_BON2E" enf-SPR_BON2 1 6 "NULL" enf-S_BON2)
    (enf--def-state states enf-S_BKEY "S_BKEY" enf-SPR_BKEY 0 10 "NULL" enf-S_BKEY2)
    (enf--def-state states enf-S_BKEY2 "S_BKEY2" enf-SPR_BKEY 32769 10 "NULL" enf-S_BKEY)
    (enf--def-state states enf-S_RKEY "S_RKEY" enf-SPR_RKEY 0 10 "NULL" enf-S_RKEY2)
    (enf--def-state states enf-S_RKEY2 "S_RKEY2" enf-SPR_RKEY 32769 10 "NULL" enf-S_RKEY)
    (enf--def-state states enf-S_YKEY "S_YKEY" enf-SPR_YKEY 0 10 "NULL" enf-S_YKEY2)
    (enf--def-state states enf-S_YKEY2 "S_YKEY2" enf-SPR_YKEY 32769 10 "NULL" enf-S_YKEY)
    (enf--def-state states enf-S_BSKULL "S_BSKULL" enf-SPR_BSKU 0 10 "NULL" enf-S_BSKULL2)
    (enf--def-state states enf-S_BSKULL2 "S_BSKULL2" enf-SPR_BSKU 32769 10 "NULL" enf-S_BSKULL)
    (enf--def-state states enf-S_RSKULL "S_RSKULL" enf-SPR_RSKU 0 10 "NULL" enf-S_RSKULL2)
    (enf--def-state states enf-S_RSKULL2 "S_RSKULL2" enf-SPR_RSKU 32769 10 "NULL" enf-S_RSKULL)
    (enf--def-state states enf-S_YSKULL "S_YSKULL" enf-SPR_YSKU 0 10 "NULL" enf-S_YSKULL2)
    (enf--def-state states enf-S_YSKULL2 "S_YSKULL2" enf-SPR_YSKU 32769 10 "NULL" enf-S_YSKULL)
    (enf--def-state states enf-S_STIM "S_STIM" enf-SPR_STIM 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_MEDI "S_MEDI" enf-SPR_MEDI 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SOUL "S_SOUL" enf-SPR_SOUL 32768 6 "NULL" enf-S_SOUL2)
    (enf--def-state states enf-S_SOUL2 "S_SOUL2" enf-SPR_SOUL 32769 6 "NULL" enf-S_SOUL3)
    (enf--def-state states enf-S_SOUL3 "S_SOUL3" enf-SPR_SOUL 32770 6 "NULL" enf-S_SOUL4)
    (enf--def-state states enf-S_SOUL4 "S_SOUL4" enf-SPR_SOUL 32771 6 "NULL" enf-S_SOUL5)
    (enf--def-state states enf-S_SOUL5 "S_SOUL5" enf-SPR_SOUL 32770 6 "NULL" enf-S_SOUL6)
    (enf--def-state states enf-S_SOUL6 "S_SOUL6" enf-SPR_SOUL 32769 6 "NULL" enf-S_SOUL)
    (enf--def-state states enf-S_PINV "S_PINV" enf-SPR_PINV 32768 6 "NULL" enf-S_PINV2)
    (enf--def-state states enf-S_PINV2 "S_PINV2" enf-SPR_PINV 32769 6 "NULL" enf-S_PINV3)
    (enf--def-state states enf-S_PINV3 "S_PINV3" enf-SPR_PINV 32770 6 "NULL" enf-S_PINV4)
    (enf--def-state states enf-S_PINV4 "S_PINV4" enf-SPR_PINV 32771 6 "NULL" enf-S_PINV)
    (enf--def-state states enf-S_PSTR "S_PSTR" enf-SPR_PSTR 32768 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PINS "S_PINS" enf-SPR_PINS 32768 6 "NULL" enf-S_PINS2)
    (enf--def-state states enf-S_PINS2 "S_PINS2" enf-SPR_PINS 32769 6 "NULL" enf-S_PINS3)
    (enf--def-state states enf-S_PINS3 "S_PINS3" enf-SPR_PINS 32770 6 "NULL" enf-S_PINS4)
    (enf--def-state states enf-S_PINS4 "S_PINS4" enf-SPR_PINS 32771 6 "NULL" enf-S_PINS)
    (enf--def-state states enf-S_MEGA "S_MEGA" enf-SPR_MEGA 32768 6 "NULL" enf-S_MEGA2)
    (enf--def-state states enf-S_MEGA2 "S_MEGA2" enf-SPR_MEGA 32769 6 "NULL" enf-S_MEGA3)
    (enf--def-state states enf-S_MEGA3 "S_MEGA3" enf-SPR_MEGA 32770 6 "NULL" enf-S_MEGA4)
    (enf--def-state states enf-S_MEGA4 "S_MEGA4" enf-SPR_MEGA 32771 6 "NULL" enf-S_MEGA)
    (enf--def-state states enf-S_SUIT "S_SUIT" enf-SPR_SUIT 32768 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PMAP "S_PMAP" enf-SPR_PMAP 32768 6 "NULL" enf-S_PMAP2)
    (enf--def-state states enf-S_PMAP2 "S_PMAP2" enf-SPR_PMAP 32769 6 "NULL" enf-S_PMAP3)
    (enf--def-state states enf-S_PMAP3 "S_PMAP3" enf-SPR_PMAP 32770 6 "NULL" enf-S_PMAP4)
    (enf--def-state states enf-S_PMAP4 "S_PMAP4" enf-SPR_PMAP 32771 6 "NULL" enf-S_PMAP5)
    (enf--def-state states enf-S_PMAP5 "S_PMAP5" enf-SPR_PMAP 32770 6 "NULL" enf-S_PMAP6)
    (enf--def-state states enf-S_PMAP6 "S_PMAP6" enf-SPR_PMAP 32769 6 "NULL" enf-S_PMAP)
    (enf--def-state states enf-S_PVIS "S_PVIS" enf-SPR_PVIS 32768 6 "NULL" enf-S_PVIS2)
    (enf--def-state states enf-S_PVIS2 "S_PVIS2" enf-SPR_PVIS 1 6 "NULL" enf-S_PVIS)
    (enf--def-state states enf-S_CLIP "S_CLIP" enf-SPR_CLIP 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_AMMO "S_AMMO" enf-SPR_AMMO 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_ROCK "S_ROCK" enf-SPR_ROCK 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BROK "S_BROK" enf-SPR_BROK 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CELL "S_CELL" enf-SPR_CELL 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CELP "S_CELP" enf-SPR_CELP 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SHEL "S_SHEL" enf-SPR_SHEL 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SBOX "S_SBOX" enf-SPR_SBOX 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BPAK "S_BPAK" enf-SPR_BPAK 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BFUG "S_BFUG" enf-SPR_BFUG 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_MGUN "S_MGUN" enf-SPR_MGUN 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CSAW "S_CSAW" enf-SPR_CSAW 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_LAUN "S_LAUN" enf-SPR_LAUN 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_PLAS "S_PLAS" enf-SPR_PLAS 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SHOT "S_SHOT" enf-SPR_SHOT 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SHOT2 "S_SHOT2" enf-SPR_SGN2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_COLU "S_COLU" enf-SPR_COLU 32768 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_STALAG "S_STALAG" enf-SPR_SMT2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BLOODYTWITCH "S_BLOODYTWITCH" enf-SPR_GOR1 0 10 "NULL" enf-S_BLOODYTWITCH2)
    (enf--def-state states enf-S_BLOODYTWITCH2 "S_BLOODYTWITCH2" enf-SPR_GOR1 1 15 "NULL" enf-S_BLOODYTWITCH3)
    (enf--def-state states enf-S_BLOODYTWITCH3 "S_BLOODYTWITCH3" enf-SPR_GOR1 2 8 "NULL" enf-S_BLOODYTWITCH4)
    (enf--def-state states enf-S_BLOODYTWITCH4 "S_BLOODYTWITCH4" enf-SPR_GOR1 1 6 "NULL" enf-S_BLOODYTWITCH)
    (enf--def-state states enf-S_DEADTORSO "S_DEADTORSO" enf-SPR_PLAY 13 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_DEADBOTTOM "S_DEADBOTTOM" enf-SPR_PLAY 18 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HEADSONSTICK "S_HEADSONSTICK" enf-SPR_POL2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_GIBS "S_GIBS" enf-SPR_POL5 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HEADONASTICK "S_HEADONASTICK" enf-SPR_POL4 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HEADCANDLES "S_HEADCANDLES" enf-SPR_POL3 32768 6 "NULL" enf-S_HEADCANDLES2)
    (enf--def-state states enf-S_HEADCANDLES2 "S_HEADCANDLES2" enf-SPR_POL3 32769 6 "NULL" enf-S_HEADCANDLES)
    (enf--def-state states enf-S_DEADSTICK "S_DEADSTICK" enf-SPR_POL1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_LIVESTICK "S_LIVESTICK" enf-SPR_POL6 0 6 "NULL" enf-S_LIVESTICK2)
    (enf--def-state states enf-S_LIVESTICK2 "S_LIVESTICK2" enf-SPR_POL6 1 8 "NULL" enf-S_LIVESTICK)
    (enf--def-state states enf-S_MEAT2 "S_MEAT2" enf-SPR_GOR2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_MEAT3 "S_MEAT3" enf-SPR_GOR3 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_MEAT4 "S_MEAT4" enf-SPR_GOR4 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_MEAT5 "S_MEAT5" enf-SPR_GOR5 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_STALAGTITE "S_STALAGTITE" enf-SPR_SMIT 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TALLGRNCOL "S_TALLGRNCOL" enf-SPR_COL1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SHRTGRNCOL "S_SHRTGRNCOL" enf-SPR_COL2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TALLREDCOL "S_TALLREDCOL" enf-SPR_COL3 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SHRTREDCOL "S_SHRTREDCOL" enf-SPR_COL4 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CANDLESTIK "S_CANDLESTIK" enf-SPR_CAND 32768 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_CANDELABRA "S_CANDELABRA" enf-SPR_CBRA 32768 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SKULLCOL "S_SKULLCOL" enf-SPR_COL6 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TORCHTREE "S_TORCHTREE" enf-SPR_TRE1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BIGTREE "S_BIGTREE" enf-SPR_TRE2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TECHPILLAR "S_TECHPILLAR" enf-SPR_ELEC 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_EVILEYE "S_EVILEYE" enf-SPR_CEYE 32768 6 "NULL" enf-S_EVILEYE2)
    (enf--def-state states enf-S_EVILEYE2 "S_EVILEYE2" enf-SPR_CEYE 32769 6 "NULL" enf-S_EVILEYE3)
    (enf--def-state states enf-S_EVILEYE3 "S_EVILEYE3" enf-SPR_CEYE 32770 6 "NULL" enf-S_EVILEYE4)
    (enf--def-state states enf-S_EVILEYE4 "S_EVILEYE4" enf-SPR_CEYE 32769 6 "NULL" enf-S_EVILEYE)
    (enf--def-state states enf-S_FLOATSKULL "S_FLOATSKULL" enf-SPR_FSKU 32768 6 "NULL" enf-S_FLOATSKULL2)
    (enf--def-state states enf-S_FLOATSKULL2 "S_FLOATSKULL2" enf-SPR_FSKU 32769 6 "NULL" enf-S_FLOATSKULL3)
    (enf--def-state states enf-S_FLOATSKULL3 "S_FLOATSKULL3" enf-SPR_FSKU 32770 6 "NULL" enf-S_FLOATSKULL)
    (enf--def-state states enf-S_HEARTCOL "S_HEARTCOL" enf-SPR_COL5 0 14 "NULL" enf-S_HEARTCOL2)
    (enf--def-state states enf-S_HEARTCOL2 "S_HEARTCOL2" enf-SPR_COL5 1 14 "NULL" enf-S_HEARTCOL)
    (enf--def-state states enf-S_BLUETORCH "S_BLUETORCH" enf-SPR_TBLU 32768 4 "NULL" enf-S_BLUETORCH2)
    (enf--def-state states enf-S_BLUETORCH2 "S_BLUETORCH2" enf-SPR_TBLU 32769 4 "NULL" enf-S_BLUETORCH3)
    (enf--def-state states enf-S_BLUETORCH3 "S_BLUETORCH3" enf-SPR_TBLU 32770 4 "NULL" enf-S_BLUETORCH4)
    (enf--def-state states enf-S_BLUETORCH4 "S_BLUETORCH4" enf-SPR_TBLU 32771 4 "NULL" enf-S_BLUETORCH)
    (enf--def-state states enf-S_GREENTORCH "S_GREENTORCH" enf-SPR_TGRN 32768 4 "NULL" enf-S_GREENTORCH2)
    (enf--def-state states enf-S_GREENTORCH2 "S_GREENTORCH2" enf-SPR_TGRN 32769 4 "NULL" enf-S_GREENTORCH3)
    (enf--def-state states enf-S_GREENTORCH3 "S_GREENTORCH3" enf-SPR_TGRN 32770 4 "NULL" enf-S_GREENTORCH4)
    (enf--def-state states enf-S_GREENTORCH4 "S_GREENTORCH4" enf-SPR_TGRN 32771 4 "NULL" enf-S_GREENTORCH)
    (enf--def-state states enf-S_REDTORCH "S_REDTORCH" enf-SPR_TRED 32768 4 "NULL" enf-S_REDTORCH2)
    (enf--def-state states enf-S_REDTORCH2 "S_REDTORCH2" enf-SPR_TRED 32769 4 "NULL" enf-S_REDTORCH3)
    (enf--def-state states enf-S_REDTORCH3 "S_REDTORCH3" enf-SPR_TRED 32770 4 "NULL" enf-S_REDTORCH4)
    (enf--def-state states enf-S_REDTORCH4 "S_REDTORCH4" enf-SPR_TRED 32771 4 "NULL" enf-S_REDTORCH)
    (enf--def-state states enf-S_BTORCHSHRT "S_BTORCHSHRT" enf-SPR_SMBT 32768 4 "NULL" enf-S_BTORCHSHRT2)
    (enf--def-state states enf-S_BTORCHSHRT2 "S_BTORCHSHRT2" enf-SPR_SMBT 32769 4 "NULL" enf-S_BTORCHSHRT3)
    (enf--def-state states enf-S_BTORCHSHRT3 "S_BTORCHSHRT3" enf-SPR_SMBT 32770 4 "NULL" enf-S_BTORCHSHRT4)
    (enf--def-state states enf-S_BTORCHSHRT4 "S_BTORCHSHRT4" enf-SPR_SMBT 32771 4 "NULL" enf-S_BTORCHSHRT)
    (enf--def-state states enf-S_GTORCHSHRT "S_GTORCHSHRT" enf-SPR_SMGT 32768 4 "NULL" enf-S_GTORCHSHRT2)
    (enf--def-state states enf-S_GTORCHSHRT2 "S_GTORCHSHRT2" enf-SPR_SMGT 32769 4 "NULL" enf-S_GTORCHSHRT3)
    (enf--def-state states enf-S_GTORCHSHRT3 "S_GTORCHSHRT3" enf-SPR_SMGT 32770 4 "NULL" enf-S_GTORCHSHRT4)
    (enf--def-state states enf-S_GTORCHSHRT4 "S_GTORCHSHRT4" enf-SPR_SMGT 32771 4 "NULL" enf-S_GTORCHSHRT)
    (enf--def-state states enf-S_RTORCHSHRT "S_RTORCHSHRT" enf-SPR_SMRT 32768 4 "NULL" enf-S_RTORCHSHRT2)
    (enf--def-state states enf-S_RTORCHSHRT2 "S_RTORCHSHRT2" enf-SPR_SMRT 32769 4 "NULL" enf-S_RTORCHSHRT3)
    (enf--def-state states enf-S_RTORCHSHRT3 "S_RTORCHSHRT3" enf-SPR_SMRT 32770 4 "NULL" enf-S_RTORCHSHRT4)
    (enf--def-state states enf-S_RTORCHSHRT4 "S_RTORCHSHRT4" enf-SPR_SMRT 32771 4 "NULL" enf-S_RTORCHSHRT)
    (enf--def-state states enf-S_HANGNOGUTS "S_HANGNOGUTS" enf-SPR_HDB1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HANGBNOBRAIN "S_HANGBNOBRAIN" enf-SPR_HDB2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HANGTLOOKDN "S_HANGTLOOKDN" enf-SPR_HDB3 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HANGTSKULL "S_HANGTSKULL" enf-SPR_HDB4 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HANGTLOOKUP "S_HANGTLOOKUP" enf-SPR_HDB5 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_HANGTNOBRAIN "S_HANGTNOBRAIN" enf-SPR_HDB6 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_COLONGIBS "S_COLONGIBS" enf-SPR_POB1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_SMALLPOOL "S_SMALLPOOL" enf-SPR_POB2 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_BRAINSTEM "S_BRAINSTEM" enf-SPR_BRS1 0 -1 "NULL" enf-S_NULL)
    (enf--def-state states enf-S_TECHLAMP "S_TECHLAMP" enf-SPR_TLMP 32768 4 "NULL" enf-S_TECHLAMP2)
    (enf--def-state states enf-S_TECHLAMP2 "S_TECHLAMP2" enf-SPR_TLMP 32769 4 "NULL" enf-S_TECHLAMP3)
    (enf--def-state states enf-S_TECHLAMP3 "S_TECHLAMP3" enf-SPR_TLMP 32770 4 "NULL" enf-S_TECHLAMP4)
    (enf--def-state states enf-S_TECHLAMP4 "S_TECHLAMP4" enf-SPR_TLMP 32771 4 "NULL" enf-S_TECHLAMP)
    (enf--def-state states enf-S_TECH2LAMP "S_TECH2LAMP" enf-SPR_TLP2 32768 4 "NULL" enf-S_TECH2LAMP2)
    (enf--def-state states enf-S_TECH2LAMP2 "S_TECH2LAMP2" enf-SPR_TLP2 32769 4 "NULL" enf-S_TECH2LAMP3)
    (enf--def-state states enf-S_TECH2LAMP3 "S_TECH2LAMP3" enf-SPR_TLP2 32770 4 "NULL" enf-S_TECH2LAMP4)
    (enf--def-state states enf-S_TECH2LAMP4 "S_TECH2LAMP4" enf-SPR_TLP2 32771 4 "NULL" enf-S_TECH2LAMP)
    (cl-loop for state across states do
             (setf (enf-state-next state) (enf--state-or-nil states (enf-state-next state))
                   (enf-state-sprite state) (aref sprites (enf-state-sprite-index state))))
    states))

(cl-defstruct (enf-mobj-info (:constructor enf--make-mobj-info) (:copier nil))
  name
  (type 0 :type fixnum)
  spawn-state
  (spawn-health 0 :type fixnum)
  see-state
  see-sound
  (reaction-time 0 :type fixnum)
  attack-sound
  pain-state
  (pain-chance 0 :type fixnum)
  pain-sound
  melee-state
  missile-state
  death-state
  x-death-state
  death-sound
  (speed-fx 0 :type fixnum)
  (radius-fx 0 :type fixnum)
  (height-fx 0 :type fixnum)
  (mass 0 :type fixnum)
  (damage 0 :type fixnum)
  active-sound
  (flags 0 :type fixnum)
  raise-state)

(defsubst enf--def-mobj-info (mobj-info index name type spawn-state spawn-health see-state reaction-time
                                        pain-state pain-chance melee-state missile-state death-state
                                        x-death-state speed radius height mass damage flags raise-state)
  (aset mobj-info index (enf--make-mobj-info :name name :type type
                                             :spawn-state spawn-state
                                             :spawn-health spawn-health :see-state see-state
                                             :reaction-time reaction-time :pain-state pain-state
                                             :pain-chance pain-chance :melee-state melee-state
                                             :missile-state missile-state :death-state death-state
                                             :x-death-state x-death-state :speed-fx (eldoom-math-fixed-from-int speed)
                                             :radius-fx (eldoom-math-fixed-from-int radius) :height-fx (eldoom-math-fixed-from-int height)
                                             :mass mass :damage damage :flags flags :raise-state raise-state)))

(defconst enf-player1-type 1)
(defun enf--make-mobj-infos (states)
  (let ((mobj-info (make-vector enf-NUMMOBJTYPES nil)))
    (enf--def-mobj-info mobj-info enf-MT_PLAYER "PLAYER" -1 enf-S_PLAY 100 enf-S_PLAY_RUN1 0 enf-S_PLAY_PAIN 255
	                enf-S_NULL enf-S_PLAY_ATK1 enf-S_PLAY_DIE1 enf-S_PLAY_XDIE1
	                0 16 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_DROPOFF enf-MF_PICKUP enf-MF_NOTDMATCH)
	                enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_POSSESSED "POSSESSED" 3004 enf-S_POSS_STND 20 enf-S_POSS_RUN1 8 enf-S_POSS_PAIN 200
	                0 enf-S_POSS_ATK1 enf-S_POSS_DIE1 enf-S_POSS_XDIE1
	                8 20 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
	                enf-S_POSS_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_SHOTGUY "SHOTGUY" 9 enf-S_SPOS_STND 30 enf-S_SPOS_RUN1 8 enf-S_SPOS_PAIN 170
	                0 enf-S_SPOS_ATK1 enf-S_SPOS_DIE1 enf-S_SPOS_XDIE1
	                8 20 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
	                enf-S_SPOS_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_VILE "VILE" 64 enf-S_VILE_STND 700 enf-S_VILE_RUN1 8 enf-S_VILE_PAIN 10
	                0 enf-S_VILE_ATK1 enf-S_VILE_DIE1 enf-S_NULL
	                15 20 56 500 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
	                enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_FIRE "FIRE" -1 enf-S_FIRE1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_UNDEAD "UNDEAD" 66 enf-S_SKEL_STND 300 enf-S_SKEL_RUN1 8 enf-S_SKEL_PAIN 100
	                enf-S_SKEL_FIST1 enf-S_SKEL_MISS1 enf-S_SKEL_DIE1 enf-S_NULL
	                10 20 56 500 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_SKEL_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_TRACER "TRACER" -1 enf-S_TRACER 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_TRACEEXP1 enf-S_NULL
	                10 11 8 100 10
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SMOKE "SMOKE" -1 enf-S_SMOKE1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_FATSO "FATSO" 67 enf-S_FATT_STND 600 enf-S_FATT_RUN1 8 enf-S_FATT_PAIN 80
	                0 enf-S_FATT_ATK1 enf-S_FATT_DIE1 enf-S_NULL
	                8 48 64 1000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_FATT_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_FATSHOT "FATSHOT" -1 enf-S_FATSHOT1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_FATSHOTX1 enf-S_NULL
	                20 6 8 100 8
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_CHAINGUY "CHAINGUY" 65 enf-S_CPOS_STND 70 enf-S_CPOS_RUN1 8 enf-S_CPOS_PAIN 170
	                0 enf-S_CPOS_ATK1 enf-S_CPOS_DIE1 enf-S_CPOS_XDIE1
	                8 20 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_CPOS_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_TROOP "TROOP" 3001 enf-S_TROO_STND 60 enf-S_TROO_RUN1 8 enf-S_TROO_PAIN 200
	                enf-S_TROO_ATK1 enf-S_TROO_ATK1 enf-S_TROO_DIE1 enf-S_TROO_XDIE1
	                8 20 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_TROO_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_SERGEANT "SERGEANT" 3002 enf-S_SARG_STND 150 enf-S_SARG_RUN1 8 enf-S_SARG_PAIN 180
	                enf-S_SARG_ATK1 0 enf-S_SARG_DIE1 enf-S_NULL
	                10 30 56 400 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_SARG_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_SHADOWS "SHADOWS" 58 enf-S_SARG_STND 150 enf-S_SARG_RUN1 8 enf-S_SARG_PAIN 180
	                enf-S_SARG_ATK1 0 enf-S_SARG_DIE1 enf-S_NULL
	                10 30 56 400 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_SHADOW enf-MF_COUNTKILL)
                        enf-S_SARG_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_HEAD "HEAD" 3005 enf-S_HEAD_STND 400 enf-S_HEAD_RUN1 8 enf-S_HEAD_PAIN 128
	                0 enf-S_HEAD_ATK1 enf-S_HEAD_DIE1 enf-S_NULL
	                8 31 56 400 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_FLOAT enf-MF_NOGRAVITY enf-MF_COUNTKILL)
                        enf-S_HEAD_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_BRUISER "BRUISER" 3003 enf-S_BOSS_STND 1000 enf-S_BOSS_RUN1 8 enf-S_BOSS_PAIN 50
	                enf-S_BOSS_ATK1 enf-S_BOSS_ATK1 enf-S_BOSS_DIE1 enf-S_NULL
	                8 24 64 1000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_BOSS_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_BRUISERSHOT "BRUISERSHOT" -1 enf-S_BRBALL1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_BRBALLX1 enf-S_NULL
	                15 6 8 100 8
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_KNIGHT "KNIGHT" 69 enf-S_BOS2_STND 500 enf-S_BOS2_RUN1 8 enf-S_BOS2_PAIN 50
	                enf-S_BOS2_ATK1 enf-S_BOS2_ATK1 enf-S_BOS2_DIE1 enf-S_NULL
	                8 24 64 1000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_BOS2_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_SKULL "SKULL" 3006 enf-S_SKULL_STND 100 enf-S_SKULL_RUN1 8 enf-S_SKULL_PAIN 256
	                0 enf-S_SKULL_ATK1 enf-S_SKULL_DIE1 enf-S_NULL
	                8 16 56 50 3
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_FLOAT enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SPIDER "SPIDER" 7 enf-S_SPID_STND 3000 enf-S_SPID_RUN1 8 enf-S_SPID_PAIN 40
	                0 enf-S_SPID_ATK1 enf-S_SPID_DIE1 enf-S_NULL
	                12 128 100 1000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BABY "BABY" 68 enf-S_BSPI_STND 500 enf-S_BSPI_SIGHT 8 enf-S_BSPI_PAIN 128
	                0 enf-S_BSPI_ATK1 enf-S_BSPI_DIE1 enf-S_NULL
	                12 64 64 600 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_BSPI_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_CYBORG "CYBORG" 16 enf-S_CYBER_STND 4000 enf-S_CYBER_RUN1 8 enf-S_CYBER_PAIN 20
	                0 enf-S_CYBER_ATK1 enf-S_CYBER_DIE1 enf-S_NULL
	                16 40 110 1000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_PAIN "PAIN" 71 enf-S_PAIN_STND 400 enf-S_PAIN_RUN1 8 enf-S_PAIN_PAIN 128
	                0 enf-S_PAIN_ATK1 enf-S_PAIN_DIE1 enf-S_NULL
	                8 31 56 400 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_FLOAT enf-MF_NOGRAVITY enf-MF_COUNTKILL)
                        enf-S_PAIN_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_WOLFSS "WOLFSS" 84 enf-S_SSWV_STND 50 enf-S_SSWV_RUN1 8 enf-S_SSWV_PAIN 170
	                0 enf-S_SSWV_ATK1 enf-S_SSWV_DIE1 enf-S_SSWV_XDIE1
	                8 20 56 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_COUNTKILL)
                        enf-S_SSWV_RAISE1)
    (enf--def-mobj-info mobj-info enf-MT_KEEN "KEEN" 72 enf-S_KEENSTND 100 enf-S_NULL 8 enf-S_KEENPAIN 256
	                enf-S_NULL enf-S_NULL enf-S_COMMKEEN enf-S_NULL
	                0 16 72 10000000 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY enf-MF_SHOOTABLE
                                enf-MF_COUNTKILL)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BOSSBRAIN "BOSSBRAIN" 88 enf-S_BRAIN 250 enf-S_NULL 8 enf-S_BRAIN_PAIN 255
	                enf-S_NULL enf-S_NULL enf-S_BRAIN_DIE1 enf-S_NULL
	                0 16 16 10000000 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BOSSSPIT "BOSSSPIT" 89 enf-S_BRAINEYE 1000 enf-S_BRAINEYESEE 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 32 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOSECTOR)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BOSSTARGET "BOSSTARGET" 87 enf-S_NULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 32 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOSECTOR)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SPAWNSHOT "SPAWNSHOT" -1 enf-S_SPAWN1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                10 6 32 100 3
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY enf-MF_NOCLIP)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SPAWNFIRE "SPAWNFIRE" -1 enf-S_SPAWNFIRE1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BARREL "BARREL" 2035 enf-S_BAR1 20 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_BEXP enf-S_NULL
	                0 10 42 100 0
	                (logior enf-MF_SOLID enf-MF_SHOOTABLE enf-MF_NOBLOOD)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_TROOPSHOT "TROOPSHOT" -1 enf-S_TBALL1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_TBALLX1 enf-S_NULL
	                10 6 8 100 3
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_HEADSHOT "HEADSHOT" -1 enf-S_RBALL1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_RBALLX1 enf-S_NULL
	                10 6 8 100 5
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_ROCKET "ROCKET" -1 enf-S_ROCKET 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_EXPLODE1 enf-S_NULL
	                20 11 8 100 20
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_PLASMA "PLASMA" -1 enf-S_PLASBALL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_PLASEXP enf-S_NULL
	                25 13 8 100 5
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BFG "BFG" -1 enf-S_BFGSHOT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_BFGLAND enf-S_NULL
	                25 13 8 100 100
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_ARACHPLAZ "ARACHPLAZ" -1 enf-S_ARACH_PLAZ 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_ARACH_PLEX enf-S_NULL
	                25 13 8 100 5
	                (logior enf-MF_NOBLOCKMAP enf-MF_MISSILE enf-MF_DROPOFF enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_PUFF "PUFF" -1 enf-S_PUFF1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_BLOOD "BLOOD" -1 enf-S_BLOOD1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_TFOG "TFOG" -1 enf-S_TFOG 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_IFOG "IFOG" -1 enf-S_IFOG 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_TELEPORTMAN "TELEPORTMAN" 14 enf-S_NULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOSECTOR)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_EXTRABFG "EXTRABFG" -1 enf-S_BFGEXP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_NOBLOCKMAP enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC0 "MISC0" 2018 enf-S_ARM1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC1 "MISC1" 2019 enf-S_ARM2 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC2 "MISC2" 2014 enf-S_BON1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC3 "MISC3" 2015 enf-S_BON2 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC4 "MISC4" 5 enf-S_BKEY 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC5 "MISC5" 13 enf-S_RKEY 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC6 "MISC6" 6 enf-S_YKEY 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC7 "MISC7" 39 enf-S_YSKULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC8 "MISC8" 38 enf-S_RSKULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC9 "MISC9" 40 enf-S_BSKULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_NOTDMATCH)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC10 "MISC10" 2011 enf-S_STIM 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC11 "MISC11" 2012 enf-S_MEDI 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC12 "MISC12" 2013 enf-S_SOUL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_INV "INV" 2022 enf-S_PINV 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC13 "MISC13" 2023 enf-S_PSTR 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_INS "INS" 2024 enf-S_PINS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC14 "MISC14" 2025 enf-S_SUIT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC15 "MISC15" 2026 enf-S_PMAP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC16 "MISC16" 2045 enf-S_PVIS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MEGA "MEGA" 83 enf-S_MEGA 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                (logior enf-MF_SPECIAL enf-MF_COUNTITEM)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_CLIP "CLIP" 2007 enf-S_CLIP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC17 "MISC17" 2048 enf-S_AMMO 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC18 "MISC18" 2010 enf-S_ROCK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC19 "MISC19" 2046 enf-S_BROK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC20 "MISC20" 2047 enf-S_CELL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC21 "MISC21" 17 enf-S_CELP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC22 "MISC22" 2008 enf-S_SHEL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC23 "MISC23" 2049 enf-S_SBOX 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC24 "MISC24" 8 enf-S_BPAK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC25 "MISC25" 2006 enf-S_BFUG 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_CHAINGUN "CHAINGUN" 2002 enf-S_MGUN 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC26 "MISC26" 2005 enf-S_CSAW 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC27 "MISC27" 2003 enf-S_LAUN 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC28 "MISC28" 2004 enf-S_PLAS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SHOTGUN "SHOTGUN" 2001 enf-S_SHOT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_SUPERSHOTGUN "SUPERSHOTGUN" 82 enf-S_SHOT2 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_SPECIAL
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC29 "MISC29" 85 enf-S_TECHLAMP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC30 "MISC30" 86 enf-S_TECH2LAMP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC31 "MISC31" 2028 enf-S_COLU 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC32 "MISC32" 30 enf-S_TALLGRNCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC33 "MISC33" 31 enf-S_SHRTGRNCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC34 "MISC34" 32 enf-S_TALLREDCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC35 "MISC35" 33 enf-S_SHRTREDCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC36 "MISC36" 37 enf-S_SKULLCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC37 "MISC37" 36 enf-S_HEARTCOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC38 "MISC38" 41 enf-S_EVILEYE 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC39 "MISC39" 42 enf-S_FLOATSKULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC40 "MISC40" 43 enf-S_TORCHTREE 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC41 "MISC41" 44 enf-S_BLUETORCH 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC42 "MISC42" 45 enf-S_GREENTORCH 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC43 "MISC43" 46 enf-S_REDTORCH 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC44 "MISC44" 55 enf-S_BTORCHSHRT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC45 "MISC45" 56 enf-S_GTORCHSHRT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC46 "MISC46" 57 enf-S_RTORCHSHRT 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC47 "MISC47" 47 enf-S_STALAGTITE 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC48 "MISC48" 48 enf-S_TECHPILLAR 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC49 "MISC49" 34 enf-S_CANDLESTIK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC50 "MISC50" 35 enf-S_CANDELABRA 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC51 "MISC51" 49 enf-S_BLOODYTWITCH 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 68 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC52 "MISC52" 50 enf-S_MEAT2 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 84 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC53 "MISC53" 51 enf-S_MEAT3 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 84 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC54 "MISC54" 52 enf-S_MEAT4 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 68 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC55 "MISC55" 53 enf-S_MEAT5 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 52 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC56 "MISC56" 59 enf-S_MEAT2 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 84 100 0
	                (logior enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC57 "MISC57" 60 enf-S_MEAT4 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 68 100 0
	                (logior enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC58 "MISC58" 61 enf-S_MEAT3 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 52 100 0
	                (logior enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC59 "MISC59" 62 enf-S_MEAT5 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 52 100 0
	                (logior enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC60 "MISC60" 63 enf-S_BLOODYTWITCH 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 68 100 0
	                (logior enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC61 "MISC61" 22 enf-S_HEAD_DIE6 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC62 "MISC62" 15 enf-S_PLAY_DIE7 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC63 "MISC63" 18 enf-S_POSS_DIE5 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC64 "MISC64" 21 enf-S_SARG_DIE6 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC65 "MISC65" 23 enf-S_SKULL_DIE6 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC66 "MISC66" 20 enf-S_TROO_DIE5 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC67 "MISC67" 19 enf-S_SPOS_DIE5 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC68 "MISC68" 10 enf-S_PLAY_XDIE9 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC69 "MISC69" 12 enf-S_PLAY_XDIE9 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC70 "MISC70" 28 enf-S_HEADSONSTICK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC71 "MISC71" 24 enf-S_GIBS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                0
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC72 "MISC72" 27 enf-S_HEADONASTICK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC73 "MISC73" 29 enf-S_HEADCANDLES 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC74 "MISC74" 25 enf-S_DEADSTICK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC75 "MISC75" 26 enf-S_LIVESTICK 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC76 "MISC76" 54 enf-S_BIGTREE 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 32 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC77 "MISC77" 70 enf-S_BBAR1 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 16 100 0
	                enf-MF_SOLID
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC78 "MISC78" 73 enf-S_HANGNOGUTS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 88 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC79 "MISC79" 74 enf-S_HANGBNOBRAIN 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 88 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC80 "MISC80" 75 enf-S_HANGTLOOKDN 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 64 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC81 "MISC81" 76 enf-S_HANGTSKULL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 64 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC82 "MISC82" 77 enf-S_HANGTLOOKUP 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 64 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC83 "MISC83" 78 enf-S_HANGTNOBRAIN 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 16 64 100 0
	                (logior enf-MF_SOLID enf-MF_SPAWNCEILING enf-MF_NOGRAVITY)
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC84 "MISC84" 79 enf-S_COLONGIBS 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_NOBLOCKMAP
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC85 "MISC85" 80 enf-S_SMALLPOOL 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_NOBLOCKMAP
                        enf-S_NULL)
    (enf--def-mobj-info mobj-info enf-MT_MISC86 "MISC86" 81 enf-S_BRAINSTEM 1000 enf-S_NULL 8 enf-S_NULL 0
	                enf-S_NULL enf-S_NULL enf-S_NULL enf-S_NULL
	                0 20 16 100 0
	                enf-MF_NOBLOCKMAP
                        enf-S_NULL)
    (cl-loop for mobj across mobj-info do
             ;; spawn state may be a null state. other states should use nil.
             (setf (enf-mobj-info-spawn-state mobj) (aref states (enf-mobj-info-spawn-state mobj))
                   (enf-mobj-info-see-state mobj) (enf--state-or-nil states (enf-mobj-info-see-state mobj))
                   (enf-mobj-info-death-state mobj) (enf--state-or-nil states (enf-mobj-info-death-state mobj))
                   (enf-mobj-info-x-death-state mobj) (enf--state-or-nil states (enf-mobj-info-x-death-state mobj))
                   (enf-mobj-info-melee-state mobj) (enf--state-or-nil states (enf-mobj-info-melee-state mobj))
                   (enf-mobj-info-missile-state mobj) (enf--state-or-nil states (enf-mobj-info-missile-state mobj))
                   (enf-mobj-info-pain-state mobj) (enf--state-or-nil states (enf-mobj-info-pain-state mobj))
                   (enf-mobj-info-raise-state mobj) (enf--state-or-nil states (enf-mobj-info-raise-state mobj))))
    mobj-info))

(defun enf--mobj-info-has-type (type info)
  (= type (enf-mobj-info-type info)))

(defun enf-for-type (mobj-infos type)
  (cl-find type mobj-infos :test #'enf--mobj-info-has-type))

(defun enf-index-for-type (mobj-infos type)
  (cl-position type mobj-infos :test #'enf--mobj-info-has-type))

(defun enf-for-index (mobj-infos index)
  "Return mobj-info for doomed thing TYPE, or nil."
  (aref mobj-infos index))

(cl-defstruct (enf-weapon-info (:constructor enf--make-weapon-info) (:copier nil))
  ammo
  up-state
  down-state
  ready-state
  attack-state
  flash-state-index
  flash-state)

(defun enf--def-weapon-info (ammo up-state down-state ready-state attack-state 
                                  flash-state-index)
  (enf--make-weapon-info :ammo ammo :up-state up-state :down-state down-state
                         :ready-state ready-state :attack-state attack-state
                         :flash-state-index flash-state-index))

(defun enf--make-weapon-infos (states)
  (let* ((weapon-infos 
          (vector (enf--def-weapon-info eldoom-globals-am_no-ammo enf-S_PUNCHUP enf-S_PUNCHDOWN
                                        enf-S_PUNCH enf-S_PUNCH1 enf-S_NULL)
                  (enf--def-weapon-info eldoom-globals-am_clip enf-S_PISTOLUP enf-S_PISTOLDOWN
                                        enf-S_PISTOL enf-S_PISTOL1 enf-S_PISTOLFLASH)
                  (enf--def-weapon-info eldoom-globals-am_shell enf-S_SGUNUP enf-S_SGUNDOWN
                                        enf-S_SGUN enf-S_SGUN1 enf-S_SGUNFLASH1)
                  (enf--def-weapon-info eldoom-globals-am_clip enf-S_CHAINUP enf-S_CHAINDOWN
                                        enf-S_CHAIN enf-S_CHAIN1 enf-S_CHAINFLASH1)
                  (enf--def-weapon-info eldoom-globals-am_missile enf-S_MISSILEUP enf-S_MISSILEDOWN
                                        enf-S_MISSILE enf-S_MISSILE1 enf-S_MISSILEFLASH1)
                  (enf--def-weapon-info eldoom-globals-am_cell enf-S_PLASMAUP enf-S_PLASMADOWN
                                        enf-S_PLASMA enf-S_PLASMA1 enf-S_PLASMAFLASH1)
                  (enf--def-weapon-info eldoom-globals-am_cell enf-S_BFGUP enf-S_BFGDOWN
                                        enf-S_BFG enf-S_BFG1 enf-S_BFGFLASH1)
                  (enf--def-weapon-info eldoom-globals-am_no-ammo enf-S_SAWUP enf-S_SAWDOWN
                                        enf-S_SAW enf-S_SAW1 enf-S_NULL)
                  (enf--def-weapon-info eldoom-globals-am_shell enf-S_DSGUNUP enf-S_DSGUNDOWN
                                        enf-S_DSGUN enf-S_DSGUN1 enf-S_DSGUNFLASH1))))
    (cl-loop for weapon across weapon-infos do
             (setf (enf-weapon-info-up-state weapon) (enf--state-or-nil states (enf-weapon-info-up-state weapon))
                   (enf-weapon-info-down-state weapon) (enf--state-or-nil states (enf-weapon-info-down-state weapon))
                   (enf-weapon-info-ready-state weapon) (enf--state-or-nil states (enf-weapon-info-ready-state weapon))
                   (enf-weapon-info-attack-state weapon) (enf--state-or-nil states (enf-weapon-info-attack-state weapon))
                   (enf-weapon-info-flash-state weapon) (enf--state-or-nil states (enf-weapon-info-flash-state-index weapon))))
    weapon-infos))

(defvar-local enf-states nil)
(defvar-local enf-mobj-infos nil)
(defvar-local enf-weapon-infos nil)

(defun enf-init (sprites)
  (setq enf-states (enf--make-states sprites)
        enf-mobj-infos (enf--make-mobj-infos enf-states)
        enf-weapon-infos (enf--make-weapon-infos enf-states)))

;; Do not try native compiling the code as it's exceeding variable tracking size limit.
;; Local Variables:
;; no-native-compile: t
;; read-symbol-shorthands: (("enf-" . "eldoom-info-"))
;; End:
(provide 'eldoom-info)
;;; eldoom-info.el ends here
