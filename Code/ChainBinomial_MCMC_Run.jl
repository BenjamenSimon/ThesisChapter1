using Plots
using DataFrames
using DataFramesMeta
using CSV
using JLD2
using RData

################
#### SET UP ####
################

cd("/home/simonb/Documents/GIT/ThesisChapter1/Code")
include("ChainBinomial_MCMC.jl")
params, data_sim_core, track_sim_core = load("../Data/cbHomo/epi_list.rds")

S0=99; I0=1; R0=0; T=100

β_tr = params[1]; δ_tr = params[2];

# d_β_core = Gamma(2.6, (1/20))
# d_δ_core = Gamma(0.4, (1/10))

d_β_core = Uniform(0, 10)
d_δ_core = Uniform(0, 10)

initial_conditions_core = [S0, I0, R0]

###################
#### EXAMPLE 1 ####
###################

data_sim_1 = deepcopy(data_sim_core)
track_sim_1 = deepcopy(track_sim_core)

d_β_1 = deepcopy(d_β_core)
d_δ_1 = deepcopy(d_δ_core)

params_dists_1 = [d_β_1, d_δ_1]
initial_conditions_1 = deepcopy(initial_conditions_core)

init_values_1 = [missing, missing]
param_infer_1 = [true, true]
data_aug_inf_1 = [true, false]
sigmas_1 = [0.5, 0.5]

Random.seed!(3)

@time begin
  res_1, other_res_1, aug_res_1,
   mSI_track_1, mIR_track_1= cb_MCMC(N_its = 1000000, data_init = data_sim_1, track_init = track_sim_1,
                                                   prior_dists = params_dists_1, params_true = init_values_1,
                                                   data_aug_infer = data_aug_inf_1, param_infer = param_infer_1,
                                                   init_cond = initial_conditions_1, T = T,
                                                   nt_up = [1,1], sigmas = sigmas_1)
end


plot(res_1[5001:end, 1])
plot(res_1[5001:end, 2])
sum(other_res_1[:, 1])/size(other_res_1, 1)
sum(other_res_1[:, 5])/size(other_res_1, 1)
println(describe(res_1))


CSV.write("../Results/cbHomo/res.csv", res_1, header=true)
CSV.write("../Results/cbHomo/other_res.csv", other_res_1, header=true)
CSV.write("../Results/cbHomo/aug_res.csv", aug_res_1, header=true)
CSV.write("../Results/cbHomo/mSI_track.csv", mSI_track_1, header=true)




###################
#### EXAMPLE 2 ####
###################

data_sim_2 = deepcopy(data_sim_core)
track_sim_2 = deepcopy(track_sim_core)

d_β_2 = deepcopy(d_β_core)
d_δ_2 = deepcopy(d_δ_core)

params_dists_2 = [d_β_2, d_δ_2]
initial_conditions_2 = deepcopy(initial_conditions_core)

init_values_2 = [missing, missing]
param_infer_2 = [true, true]
data_aug_inf_2 = [false, false]
sigmas_2 = [0.5, 0.5]

Random.seed!(4)

@time begin
  res_2, other_res_2, aug_res_2,
   mSI_track_2, mIR_track_2= cb_MCMC(N_its = 1000000, data_init = data_sim_2, track_init = track_sim_2,
                                                   prior_dists = params_dists_2, params_true = init_values_2,
                                                   data_aug_infer = data_aug_inf_2, param_infer = param_infer_2,
                                                   init_cond = initial_conditions_2, T = T,
                                                   nt_up = [1,1], sigmas = sigmas_2)
end


plot(res_2[5001:end, 1])
plot(res_2[5001:end, 2])
sum(other_res_2[:, 1])/size(other_res_2, 1)
sum(other_res_2[:, 5])/size(other_res_2, 1)
println(describe(res_2))


CSV.write("../Results/cbHomo/res_dI_known.csv", res_2, header=true)
CSV.write("../Results/cbHomo/other_res_dI_known.csv", other_res_2, header=true)
CSV.write("../Results/cbHomo/aug_res_dI_known.csv", aug_res_2, header=true)
CSV.write("../Results/cbHomo/mSI_track_dI_known.csv", mSI_track_2, header=true)
