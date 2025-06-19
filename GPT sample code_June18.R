# required packages

library(dplyr)

# Set simulation
n_iter <- 10000
dt <- 1  # per second 
alpha <- 0.265 # should be updated 
N50 <- 5.597 #should be updated 


# Set steps (step 1:loading laundry to washer/ step2: exposure after loading washer/ Step 3: loading laundry to dryer/ Step 4: exposure after loading dryer)
steps <- list(
  list(name = "Step 1", duration = 60, params = list(
    H_fh = function() rnorm(1, 0.02, 0.005),
    TE_fh = function() runif(1, 0.3, 0.4),
    TE_hf = function() runif(1, 0.2, 0.3),
    TE_hm = function() runif(1, 0.3, 0.5),
    TE_fm = function() runif(1, 0.3, 0.5),
    kf = function() rexp(1, rate = 1/0.0052) / 60,
    kh = function() rexp(1, rate = 1/0.27) / 60
  )),
  list(name = "Step 2", duration = 900, params = list(
    H_fh = function() rnorm(1, 0.01, 0.003),
    TE_fh = function() runif(1, 0.1, 0.2),
    TE_hf = function() runif(1, 0.1, 0.2),
    TE_hm = function() runif(1, 0.2, 0.4),
    TE_fm = function() runif(1, 0.2, 0.4),
    kf = function() rexp(1, rate = 1/0.003) / 60,
    kh = function() rexp(1, rate = 1/0.2) / 60
  )),
  list(name = "Step 3", duration = 60, params = list(
    H_fh = function() rnorm(1, 0.025, 0.004),
    TE_fh = function() runif(1, 0.35, 0.45),
    TE_hf = function() runif(1, 0.25, 0.35),
    TE_hm = function() runif(1, 0.4, 0.5),
    TE_fm = function() runif(1, 0.4, 0.5),
    kf = function() rexp(1, rate = 1/0.006) / 60,
    kh = function() rexp(1, rate = 1/0.25) / 60
  )),
  list(name = "Step 4", duration = 900, params = list(
    H_fh = function() rnorm(1, 0.012, 0.003),
    TE_fh = function() runif(1, 0.2, 0.3),
    TE_hf = function() runif(1, 0.15, 0.25),
    TE_hm = function() runif(1, 0.3, 0.5),
    TE_fm = function() runif(1, 0.3, 0.5),
    kf = function() rexp(1, rate = 1/0.004) / 60,
    kh = function() rexp(1, rate = 1/0.3) / 60
  ))
)

# 공통 면적 (고정)
S_fh <- 3
S_hm <- 3
S_fm <- 3
H_hm <- 0.002
H_fm <- 0.002

# 결과 저장
results <- list()

set.seed(123)
for (sim in 1:n_iter) {
  C_fomite <- 1
  C_hand <- 0
  dose <- 0
  sim_data <- data.frame()
  
  time_offset <- 0  # 전체 시뮬레이션 시간
  
  for (step in steps) {
    dur <- step$duration
    p <- step$params
    
    # 샘플링된 파라미터 고정
    H_fh <- p$H_fh()
    TE_fh <- p$TE_fh()
    TE_hf <- p$TE_hf()
    TE_hm <- p$TE_hm()
    TE_fm <- p$TE_fm()
    kf <- p$kf()
    kh <- p$kh()
    
    for (t in 0:(dur - 1)) {
      time <- time_offset + t
      
      # decay
      C_fomite <- C_fomite * exp(-kf * dt)
      C_hand <- C_hand * exp(-kh * dt)
      
      # 양방향 전이
      if (runif(1) < H_fh * dt) {
        delta_fh <- TE_fh * max(C_fomite - C_hand, 0)
        C_hand <- C_hand + delta_fh * S_fh
        C_fomite <- C_fomite - delta_fh * S_fh
        
        delta_hf <- TE_hf * max(C_hand - C_fomite, 0)
        C_fomite <- C_fomite + delta_hf * S_fh
        C_hand <- C_hand - delta_hf * S_fh
      }
      
      # hand → mouth
      if (runif(1) < H_hm * dt) {
        dose <- dose + TE_hm * C_hand * S_hm
        C_hand <- C_hand * (1 - TE_hm)
      }
      
      # fomite → mouth
      if (runif(1) < H_fm * dt) {
        dose <- dose + TE_fm * C_fomite * S_fm
        C_fomite <- C_fomite * (1 - TE_fm)
      }
      
      risk <- if (dose > 0) {
        1 - (1 + (dose / N50) * (2^(1 / alpha) - 1))^(-alpha)
      } else 0
      
      sim_data <- rbind(sim_data, data.frame(
        simulation = sim,
        time_sec = time,
        step = step$name,
        C_fomite = C_fomite,
        C_hand = C_hand,
        dose = dose,
        risk = risk
      ))
    }
    time_offset <- time_offset + dur
  }
  results[[sim]] <- sim_data
}

# 모든 결과 결합
df_all <- bind_rows(results)
