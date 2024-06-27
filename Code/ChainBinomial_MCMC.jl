using Distributions
using Random
using Plots
using DataFrames
using DataFramesMeta
using LinearAlgebra
using StatsBase


##############################
### LIKELIHOOD + POSTERIOR ###
##############################

function p_inf(I_t, N_t, β)
  p_inf_t = 1-exp(β * - (I_t/N_t))
  return(p_inf_t)
end

function init_cond_llh(init_conditions, params, p_rem, track_in)

  p_inf_init = p_inf(init_conditions[2], sum(init_conditions), params[1])

  prob_new_I_1 = logpdf(Binomial(init_conditions[1], p_inf_init), track_in[1, 1])
  prob_new_R_1 = logpdf(Binomial(init_conditions[2], p_rem), track_in[1, 2])

  return(prob_new_I_1 + prob_new_R_1)
end

function bulk_llh(t, data_in, track_in, params, p_rem)

  p_inf_t = p_inf(data_in[(t-1), 2], sum(data_in[(t-1), :]), params[1])

  prob_new_I = logpdf(Binomial(convert(Int64, data_in[(t-1), 1]), p_inf_t), track_in[t, 1])
  prob_new_R = logpdf(Binomial(convert(Int64, data_in[(t-1), 2]), p_rem), track_in[t, 2])

  return(prob_new_I + prob_new_R)
end

function llh(data_in, track_in, params, init_conditions)

  p_rem = 1 - exp(-params[2])

  llh = init_cond_llh(init_conditions, params, p_rem, track_in)

  for t in 2:size(data_in, 1)
    llh = llh + bulk_llh(t, data_in, track_in, params, p_rem)
  end

  return(llh)
end

function posterior(data_in, track_in, params, prior_dists, init_conditions)

  loglh = llh(data_in, track_in, params, init_conditions)

  priors = logpdf(prior_dists[1], params[1]) +
           logpdf(prior_dists[2], params[2])

  post = loglh + priors

  return(post)
end


#################
### PROPOSALS ###
#################

function propose_params(log_params_cur, param_index, sigma_param)

  ##############################################
  ### Initialise the parameters and epidemic ###
  ##############################################

  log_params_draw = deepcopy(log_params_cur)

  ##########################
  ### Draw the paramters ###
  ##########################

  log_params_draw[param_index] = rand(Normal(log_params_cur[param_index], sigma_param ))

  return(log_params_draw, 0.0)
        #log_params_draw, log_q_ratio
end


function move_find_t_valid(track, event, mint, maxt)

  choosing_a_t = Int64[]

  for t in mint:maxt
    if track[t, event] > 0
      push!(choosing_a_t, t)
    end
  end

  return(choosing_a_t)
end

function move_update_states(data, t, event, Δ, num_event_move_t)

  sgnΔ = sign(Δ)
  A = convert(Int64, (1 - sign(Δ))/2)
  B = 1-A

  for k in (t+(A*Δ)):(t-1+(B*Δ))

    data[k , event] += sgnΔ * num_event_move_t

    data[k , (event+1)] -= sgnΔ * num_event_move_t
  end

  return(data)
end

function move_update_track(track, t, event, Δ, num_event_move_t)

  track[t , event] -= num_event_move_t
  track[(t+Δ) , event] += num_event_move_t

  return(track)
end

function move_calc_q_prime_given_cur(choosing_a_t)

  prob = 1/size(choosing_a_t, 1)

  return(log(prob))
end

function move_calc_q_cur_given_prime(track_prime, event, mint, maxt)

  choosing_a_t_prime = move_find_t_valid(track_prime, event, mint, maxt)

  prob = 1/size(choosing_a_t_prime, 1)

  return(log(prob))
end

function propose_move(data_in, track_in, event, T, init_conditions, nt_up)

  data_cur = deepcopy(data_in)
  track_cur = deepcopy(track_in)

  data_prime = deepcopy(data_in)
  track_prime = deepcopy(track_in)

  log_q_ratio = 0.

  #### Generate the updates ####

  Δ_array = rand([-1, 1], nt_up)

  for Δi in Δ_array

    #### Choose a timestep ####

      if Δi == 1 # move forward in time
        choosing_a_t = move_find_t_valid(track_cur, event, 1, (T-1))
      end

      if Δi == -1 # move backward in time
        choosing_a_t = move_find_t_valid(track_cur, event, 2, T)
      end

    #### Generate update ####

      t = sample(choosing_a_t, 1, replace = false)[1]

      # Number of [] to [] events at time t
      num_event_t = track_cur[t, event]

      # Generate a number to be moved
      num_event_move_t = 1

    #### Generate new states ####

      # Update the events
      track_prime = move_update_track(track_cur, t, event, Δi, num_event_move_t)

      # Update the states
      data_prime = move_update_states(data_cur, t, event, Δi, num_event_move_t)


      #### Quick check for validity ####

      A = convert(Int64, (1 - sign(Δi))/2)
      B = 1-A

      # Early return: Invalid Update
      for j in 1:3
        for k in (t+(A*Δi)):(t+(B*Δi))

          if data_prime[k,j] < 0
            Move_track = [t, 0, 3, Δi, num_event_move_t]
            # data_in, track_in, log_q_ratio, Move_track
            return(data_in, track_in, -Inf, Move_track)
          end

        end
      end

      ##### Calculate the log q ratio (proposal density ratio) ####

      q_prime_given_cur = move_calc_q_prime_given_cur(choosing_a_t)

      if Δi == 1 # move forward in time
        q_cur_given_prime = move_calc_q_cur_given_prime(track_prime, event, 1, (T-1))
      end

      if Δi == -1 # move backward in time
        q_cur_given_prime = move_calc_q_cur_given_prime(track_prime, event, 2, (T-1))
      end

      log_q_ratio += (q_cur_given_prime - q_prime_given_cur)

      #### Update intermediate arrays ####

      data_cur = deepcopy(data_prime)
      track_cur = deepcopy(track_prime)

    end # end of for Δi in Δ_array

  # ELSE

  Move_track = [0, 0, 0, -1, -1]

    # data_in, track_in, log_q_ratio, Move_track
  return(data_prime, track_prime, log_q_ratio, Move_track)
end


####################
### MH FUNCTIONS ###
####################

### Metropolis Hastings functions ####

function mh_accept_ratio(post, post_prime, log_q_ratio)

  log_α_ratio =  min( 0,  (post_prime) - (post) + log_q_ratio )

  return(log_α_ratio)
end

function mult_MH_accept(post, post_prime, log_q_ratio, log_params_cur, log_params_draw)
  alpha =  (sum(log_params_draw) + post_prime) - (sum(log_params_cur) + post) + log_q_ratio

  return( min(0, alpha) )
end

# MH function for parameters
function metropolis_hastings_step_params(data_cur, track_cur,
                                          post_cur, prior_dists, init_cond,
                                          log_params_cur, param_index, sigma_param)

  # Propose an update
  log_params_draw, log_q_ratio = propose_params(log_params_cur, param_index, sigma_param)

  # Early return: Update is invalid
  if isfinite(log_q_ratio) == false
                    # is_accepted, log_α_ratio, post_cur, post_prime
    return(log_params_cur, [false, -Inf, post_cur, -Inf])
  end

  # Calculate new llh arrays and posteriors
  post_prime = posterior(data_cur, track_cur, exp.(log_params_draw), prior_dists, init_cond)


  # Calculate MH acceptance probability
  log_α_ratio = mult_MH_accept(post_cur, post_prime, log_q_ratio, log_params_cur, log_params_draw)
  is_accepted = false

  # Accept/Reject
  accept_test = log(rand())
  if accept_test < log_α_ratio  #If yes:

    post_cur = deepcopy(post_prime)
    log_params_cur = deepcopy(log_params_draw)

    is_accepted = true
  end

  return(log_params_cur, post_cur, [is_accepted, log_α_ratio, post_cur, post_prime])
end

# MH function for general data augmentation
function metropolis_hastings_step_aug(data_cur, track_cur, post_cur, prior_dists, log_params_cur, event, init_cond, T, nt_up)

  # Propose an update
  data_prime, track_prime, log_q_ratio, update_tracker = propose_move(data_cur, track_cur, event, T, init_cond, nt_up)

  # Early return: Update is invalid
  if isfinite(log_q_ratio) == false
                                  # is_accepted, log_α_ratio, post_cur, post_prime
    return(data_cur, track_cur, post_cur, [false, -Inf, post_cur, -Inf], update_tracker)
  end

  # Calculate new posteriors
  post_prime = posterior(data_prime, track_prime, exp.(log_params_cur), prior_dists, init_cond)

  # Calculate MH acceptance probability
  log_α_ratio = mh_accept_ratio(post_cur, post_prime, log_q_ratio)
  is_accepted = false

  # Accept/Reject
  accept_test = log(rand())
  if accept_test < log_α_ratio  #If yes:

    data_cur = deepcopy(data_prime)
    track_cur = deepcopy(track_prime)

    post_cur = deepcopy(post_prime)

    update_tracker[2:3] = [1.,1.]

    is_accepted = true
  end

  return(data_cur, track_cur, post_cur, [is_accepted, log_α_ratio, post_cur, post_prime], update_tracker)
end


#####################
### Adaptive MCMC ###
#####################

function Initialise(data_in, track_in, params_true, prior_dists, init_conditions)

  # Current values
  β_cur, δ_cur = params_true
  params_draw = [0.0, 0.0]

  # True values
  β_true, δ_true = params_true

  # Prior distributions
  d_β, d_δ = prior_dists


  arb = true
  while arb == true
    # Initialise parameters
    if ismissing(β_true)
      β_cur = rand(d_β)
    end
    if ismissing(δ_true)
      δ_cur = rand(d_δ)
    end

    # Current draws
    params_draw = [β_cur, δ_cur]

    #Calculate the log-likelihood

    init_llh = llh(data_in, track_in, params_draw, init_conditions)

    if init_llh != -Inf
      break
    end
  end

  println(" ", "<", "Initialised!", ">")

  return(params_draw)
end

function create_results_arrays(N_its, its_per_frame)

  res = Array{Float64}(undef, N_its, 3)
  # :β, :δ :sample

  other_res = Array{Float64}(undef, N_its, 9)
  # :acc_β, :log_α_ratio_β, :post_β, :post_prime_β,
  # :acc_δ, :log_α_ratio_δ, :post_δ, :post_prime_δ, :sample

  aug_res = Array{Float64}(undef, N_its, 8)
  # [is_accepted, log_α_ratio, post, post_prime] for
  # Move SI; Move IR

  move_SI_tracker = Array{Float64}(undef, N_its, 5)
  # :t, :is_accepted, :reason,
  # :Δ_time, :num_moved

  move_IR_tracker = Array{Float64}(undef, N_its, 5)
  # :t, :is_accepted, :reason,
  # :Δ_time, :num_moved

  no_of_saves = convert(Int64, N_its/its_per_frame)
  SIM_tracker = Array{Array{}}(undef, no_of_saves)
  EVENTS_tracker = Array{Array{}}(undef, no_of_saves)

  return(res, other_res, aug_res, move_SI_tracker, move_IR_tracker, SIM_tracker, EVENTS_tracker)
end

function rename_results_arrays(res, other_res, aug_res, move_SI_tracker, move_IR_tracker)

  res[:, 1:2] = exp.(res[:, 1:2])

  res = DataFrame(res, :auto)
  rename!(res, [:β, :δ, :sample])

  other_res = DataFrame(other_res, :auto)
  rename!(other_res, [:acc_β, :log_α_ratio_β, :post_β, :post_prime_β,
                      :acc_δ, :log_α_ratio_δ, :post_δ, :post_prime_δ, :sample])

  aug_res = DataFrame(aug_res, :auto)
  rename!(aug_res, [:is_accepted_move_SI, :log_α_ratio_move_SI, :post_move_SI, :post_prime_move_SI,
                      :is_accepted_move_IR, :log_α_ratio_move_IR, :post_move_IR, :post_prime_move_IR])

  move_SI_tracker = DataFrame(move_SI_tracker, :auto)
  rename!(move_SI_tracker, [:t, :is_accepted, :reason,
                            :Δ_time, :num_moved])

  move_IR_tracker = DataFrame(move_IR_tracker, :auto)
  rename!(move_IR_tracker, [:t, :is_accepted, :reason,
                            :Δ_time, :num_moved])

  return(res, other_res, aug_res, move_SI_tracker, move_IR_tracker)
end

function cb_MCMC(;N_its, data_init, track_init,
                                    prior_dists, params_true,
                                    data_aug_infer, param_infer,
                                    init_cond, T, nt_up, sigmas)

  ##############################################
  ### Initialise the parameters and epidemic ###
  ##############################################

      params_cur = Initialise(data_init, track_init, params_true, prior_dists, init_cond)

      log_params_cur = log.(params_cur)

      # println(" ", "Calculated log params cur")

  ########################
  ### Results matrices ###
  ########################

      res, other_res, aug_res, move_SI_tracker, move_IR_tracker,
        SIM_tracker, EVENTS_tracker = create_results_arrays(N_its, 100)

      # println(" ", "Created results arrays")

  ##########################
  ### Functional objects ###
  ##########################

      it = 1

      data_cur = data_init
      track_cur = track_init

      post_cur = posterior(data_cur, track_cur, params_cur, prior_dists, init_cond)

      # println(" ", "Calculated posterior")

  ###########################
  ### ~~ THE ALGORITHM ~~ ###
  ###########################

      while it <= N_its

        # println(" ", "~~{", " it = ", it, " }~~")

        #####################
        ### MH step for β ###
        #####################

          mh_res_β = [-Inf, -Inf, Inf, Inf]

          if param_infer[1] == true

            log_params_cur, post_cur, mh_res_β =
                      metropolis_hastings_step_params(data_cur, track_cur,
                                                      post_cur, prior_dists, init_cond,
                                                      log_params_cur, 1, sigmas[1])
          end

          # println(" ", "Proposed β" )

        #####################
        ### MH step for δ ###
        #####################

          mh_res_δ = [-Inf, -Inf, Inf, Inf]

          if param_infer[2] == true

            log_params_cur, post_cur, mh_res_δ =
                      metropolis_hastings_step_params(data_cur, track_cur,
                                                      post_cur, prior_dists, init_cond,
                                                      log_params_cur, 2, sigmas[2])
          end

          # println(" ", "Proposed δ" )

        ###############################
        ### Data Augmentation Steps ###
        ###############################

        ###################################
        ### Move S→I event through time ###
        ###################################

          mh_res_move_SI = [-Inf, -Inf, Inf, Inf]
          move_SI_track = [-Inf, -Inf, -Inf, -Inf, -Inf]

          if data_aug_infer[1] == true

            data_cur, track_cur, post_cur, mh_res_move_SI, move_SI_track =
                  metropolis_hastings_step_aug(data_cur, track_cur, post_cur, prior_dists, log_params_cur, 1, init_cond, T, nt_up[1])
          end

          # println(" ", "Proposed dE update" )

        ###################################
        ### Move I→R event through time ###
        ###################################

          mh_res_move_IR = [-Inf, -Inf, Inf, Inf]
          move_IR_track = [-Inf, -Inf, -Inf, -Inf, -Inf]

          if data_aug_infer[2] == true

            data_cur, track_cur, post_cur, mh_res_move_IR, move_IR_track =
                  metropolis_hastings_step_aug(data_cur, track_cur, post_cur, prior_dists, log_params_cur, 2, init_cond, T, nt_up[2])

          end

          # println(" ", "Proposed dI update" )

        ##########################
        ### Record the results ###
        ##########################

          # Record parameters
          res[it,:] = [log_params_cur; it]

          # Record other
          other_res[it,:] = [mh_res_β[1], mh_res_β[2], mh_res_β[3], mh_res_β[4],
                             mh_res_δ[1], mh_res_δ[2], mh_res_δ[3], mh_res_δ[4], it]

          # Record data aug
          aug_res[it, :] = [mh_res_move_SI ; mh_res_move_IR]

          # Record update tracking data
          move_SI_tracker[it, :] = move_SI_track
          move_IR_tracker[it, :] = move_IR_track

          # println(" ", "Recorded the results" )

          # if rem(it, 100) == 0
          #   SIM_count = convert(Int64, it/100)
          #   SIM_tracker[SIM_count] = data_cur
          #   EVENTS_tracker[SIM_count] = track_cur
          # end

          if rem(it, 10000) == 0
            println("it = ", it)
            #println("sum(dE)_cur = ", sum(track_cur[:, 1]), " and sum(dI)_cur = ", sum(track_cur[:, 2]))
          end

          it = it + 1

      end #end of while

  # println(" ", "Finished while loop" )

  res, other_res, aug_res, move_SI_tracker, move_IR_tracker = rename_results_arrays(res, other_res, aug_res, move_SI_tracker, move_IR_tracker)

  return(res, other_res, aug_res, move_SI_tracker, move_IR_tracker) #, SIM_tracker, EVENTS_tracker)
end
