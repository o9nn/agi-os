import logging
import numpy as np
from typing import Dict, List, Tuple
from dataclasses import dataclass, field
from enum import IntEnum
from collections import deque
from emotional_dynamics import EmotionalDynamics, CoreEmotion
try:
    from julia import Julia
    from julia import Main as jl
    jl_installed = True
except ImportError:
    jl_installed = False
    logging.warning('Julia or PyJulia not installed. Falling back to Python implementation.')
class DETEmotion(IntEnum):
    INTEREST = 0
    EXCITEMENT = 1
    ANGER = 2
    CONTEMPT = 3
    DISGUST = 4
    FEAR = 5
    SHAME = 6
    SHYNESS = 7
    GUILT = 8
    DESIRE = 9
    LOVE = 10
    TENDERNESS = 11
    DISTRESS = 12
    SADNESS = 13
    JOY = 14
    AMUSEMENT = 15
    SURPRISE = 16
@dataclass
class EmotionalScript:
    name: str
    triggering_emotions: List[DETEmotion]
    cognitions: List[str]
    behavioral_responses: List[str]
    intensity_modifier: float = 1.0
    recency_weight: float = 0.0
    def matches_emotions(self, emotions: List[DETEmotion], threshold: int=1) -> bool:
        matches = set(self.triggering_emotions).intersection(set(emotions))
        return len(matches) >= threshold
@dataclass
class DETState:
    det_emotions: np.ndarray = field(default_factory=lambda: np.array([0.1] * 17))
    transition_matrix: np.ndarray = None
    cognitive_factors: Dict[str, float] = field(default_factory=dict)
    active_scripts: List[EmotionalScript] = field(default_factory=list)
    regulation_capacity: float = 0.5
    script_history: deque = field(default_factory=lambda: deque(maxlen=10))
    def __post_init__(self):
        if self.transition_matrix is None:
            self.transition_matrix = np.zeros((17, 17))
            self.transition_matrix[DETEmotion.INTEREST, DETEmotion.EXCITEMENT] = 0.7
            self.transition_matrix[DETEmotion.ANGER, DETEmotion.DISGUST] = 0.5
            self.transition_matrix[DETEmotion.ANGER, DETEmotion.CONTEMPT] = 0.5
            self.transition_matrix[DETEmotion.FEAR, DETEmotion.SHAME] = 0.4
            self.transition_matrix[DETEmotion.FEAR, DETEmotion.GUILT] = 0.4
            self.transition_matrix[DETEmotion.DISTRESS, DETEmotion.SADNESS] = 0.8
            self.transition_matrix[DETEmotion.JOY, DETEmotion.AMUSEMENT] = 0.7
        if not self.cognitive_factors:
            self.cognitive_factors = {'valence': 0.0, 'arousal': 0.5, 'control': 0.5, 'certainty': 0.5, 'effort': 0.5, 'attention': 0.5}
class DifferentialEmotionSystem:
    def __init__(self, use_julia: bool=True):
        self.logger = logging.getLogger(__name__)
        self.use_julia = use_julia and jl_installed
        self.base_dynamics = EmotionalDynamics(use_julia=use_julia)
        self.script_library = self._create_script_library()
        if self.use_julia:
            self._setup_julia_extensions()
    def _setup_julia_extensions(self):
        try:
            julia_code = '\n            module DifferentialEmotion\n            \n            using DifferentialEquations\n            using LinearAlgebra\n            using Distributions\n            \n            # Cognitive appraisal simulation\n            function simulate_appraisal(emotions::Vector{Float64}, \n                                       cognitive_factors::Dict{String, Float64}, \n                                       time_span::Tuple{Float64, Float64})\n                # Create a system of ODEs for cognitive-emotion interactions\n                function appraisal_dynamics!(du, u, p, t)\n                    emotions, cog_factors = p\n                    \n                    # Emotion components (first 17 elements of u)\n                    e = @view u[1:17]\n                    \n                    # Cognitive factors (remaining elements)\n                    cog = @view u[18:end]\n                    \n                    # Emotion dynamics\n                    for i in 1:17\n                        # Natural decay\n                        decay = -0.2 * e[i]\n                        \n                        # Cognitive influence on emotion\n                        cognitive_influence = 0.0\n                        \n                        # Valence affects emotional intensity differently\n                        if i in [1, 2, 9, 10, 11, 14, 15, 16]  # Positive emotions\n                            cognitive_influence += 0.3 * cog[1]  # Valence boosts positive emotions\n                        else  # Negative emotions\n                            cognitive_influence += -0.3 * cog[1]  # Valence reduces negative emotions\n                        end\n                        \n                        # Arousal amplifies all emotions\n                        cognitive_influence += 0.2 * cog[2] * e[i]\n                        \n                        # Higher control reduces fear, shame, guilt\n                        if i in [5, 6, 7, 8]\n                            cognitive_influence += -0.3 * cog[3]\n                        end\n                        \n                        # Apply emotional regulation\n                        regulation = -0.2 * e[i] * cog[6]  # Attention as regulation\n                        \n                        # Combine effects\n                        du[i] = decay + cognitive_influence + regulation\n                    end\n                    \n                    # Cognitive dynamics\n                    # Valence\n                    du[18] = 0.1 * (sum(e[i] for i in [1, 2, 9, 10, 11, 14, 15, 16]) - \n                                    sum(e[i] for i in [3, 4, 5, 6, 7, 8, 12, 13])) - 0.1 * cog[1]\n                    \n                    # Arousal\n                    du[19] = 0.2 * (sum(e[i] for i in [1, 2, 3, 5, 9, 14, 16]) - \n                                   sum(e[i] for i in [7, 13])) - 0.1 * cog[2]\n                                   \n                    # Control\n                    du[20] = 0.1 * (sum(e[i] for i in [1, 3, 4]) - \n                                   sum(e[i] for i in [5, 6, 7, 8, 12, 13])) - 0.1 * cog[3]\n                    \n                    # Certainty\n                    du[21] = -0.3 * e[16] - 0.2 * e[5] - 0.1 * cog[4]\n                    \n                    # Effort\n                    du[22] = 0.2 * (e[1] + e[2]) + 0.1 * e[3] - 0.1 * cog[5]\n                    \n                    # Attention\n                    du[23] = 0.2 * (e[1] + e[16]) - 0.1 * cog[6]\n                end\n                \n                # Create initial state (emotions + cognitive factors)\n                initial_state = vcat(\n                    emotions, \n                    [cognitive_factors["valence"], \n                     cognitive_factors["arousal"],\n                     cognitive_factors["control"],\n                     cognitive_factors["certainty"],\n                     cognitive_factors["effort"],\n                     cognitive_factors["attention"]]\n                )\n                \n                # Set up parameters\n                params = (emotions, cognitive_factors)\n                \n                # Create ODE problem\n                prob = ODEProblem(appraisal_dynamics!, initial_state, time_span, params)\n                \n                # Solve ODE\n                sol = solve(prob, Tsit5(), reltol=1e-6, abstol=1e-6)\n                \n                # Return final state\n                final_state = sol.u[end]\n                \n                # Split into emotions and cognitive factors\n                final_emotions = final_state[1:17]\n                final_cognitive = Dict(\n                    "valence" => final_state[18],\n                    "arousal" => final_state[19],\n                    "control" => final_state[20],\n                    "certainty" => final_state[21],\n                    "effort" => final_state[22],\n                    "attention" => final_state[23]\n                )\n                \n                return (final_emotions, final_cognitive)\n            end\n            \n            # Simulate emotion regulation\n            function simulate_regulation(emotions::Vector{Float64}, \n                                        regulation_capacity::Float64,\n                                        regulation_target::Int,\n                                        regulation_type::String,\n                                        time_span::Tuple{Float64, Float64})\n                # Different regulation strategies affect emotions differently\n                function regulation_dynamics!(du, u, p, t)\n                    reg_capacity, reg_target, reg_type = p\n                    \n                    for i in 1:length(u)\n                        # Natural decay\n                        decay = -0.1 * u[i]\n                        \n                        # Regulation effect\n                        regulation = 0.0\n                        \n                        if reg_type == "suppression"\n                            # Suppression reduces target emotion but increases others\n                            if i == reg_target + 1\n                                regulation = -0.3 * reg_capacity\n                            else\n                                regulation = 0.05 * reg_capacity\n                            end\n                        elseif reg_type == "reappraisal"\n                            # Reappraisal reduces negative emotions and increases positive\n                            if i in [3, 4, 5, 6, 7, 8, 12, 13]  # Negative emotions\n                                regulation = -0.2 * reg_capacity\n                            elseif i in [1, 2, 9, 10, 11, 14, 15]  # Positive emotions\n                                regulation = 0.1 * reg_capacity\n                            end\n                        elseif reg_type == "distraction"\n                            # Distraction reduces all emotions slightly\n                            regulation = -0.1 * reg_capacity\n                        end\n                        \n                        # Combine effects\n                        du[i] = decay + regulation\n                    end\n                end\n                \n                # Create ODE problem\n                params = (regulation_capacity, regulation_target, regulation_type)\n                prob = ODEProblem(regulation_dynamics!, emotions, time_span, params)\n                \n                # Solve ODE\n                sol = solve(prob, Tsit5(), reltol=1e-6, abstol=1e-6)\n                \n                # Return final state\n                return sol.u[end]\n            end\n            \n            # Extract emotional scripts based on emotion patterns\n            function extract_scripts(emotions::Vector{Float64}, \n                                     scripts::Vector{Vector{Int}},\n                                     script_thresholds::Vector{Float64})\n                activated_scripts = Int[]\n                \n                for i in 1:length(scripts)\n                    script = scripts[i]\n                    threshold = script_thresholds[i]\n                    \n                    # Check if script emotions are active\n                    active_emotions = sum(emotions[j] > 0.2 for j in script)\n                    if active_emotions / length(script) >= threshold\n                        push!(activated_scripts, i)\n                    end\n                end\n                \n                return activated_scripts\n            end\n            \n            end # module\n            '
            jl.eval(julia_code)
            self.det_julia = jl.DifferentialEmotion
            self.logger.info('Successfully loaded Julia Differential Emotion extension')
        except Exception as e:
            self.logger.error(f'Failed to set up Julia extensions for DET: {e}')
            self.use_julia = False
    def _create_script_library(self) -> List[EmotionalScript]:
        scripts = []
        scripts.append(EmotionalScript(name='Exploration', triggering_emotions=[DETEmotion.INTEREST, DETEmotion.EXCITEMENT], cognitions=['This is novel', 'I want to learn more', 'This is fascinating'], behavioral_responses=['Approach', 'Investigate', 'Ask questions']))
        scripts.append(EmotionalScript(name='Escape', triggering_emotions=[DETEmotion.FEAR, DETEmotion.SHAME], cognitions=['This is dangerous', 'I need to get away', "I'm not safe"], behavioral_responses=['Retreat', 'Hide', 'Freeze', 'Seek safety']))
        scripts.append(EmotionalScript(name='Attack', triggering_emotions=[DETEmotion.ANGER, DETEmotion.CONTEMPT, DETEmotion.DISGUST], cognitions=['This is an obstacle', 'This is unfair', 'This threatens my goals'], behavioral_responses=['Confront', 'Remove obstacle', 'Express disapproval']))
        scripts.append(EmotionalScript(name='Celebration', triggering_emotions=[DETEmotion.JOY, DETEmotion.AMUSEMENT], cognitions=['This is good', 'I succeeded', 'Life is enjoyable'], behavioral_responses=['Smile', 'Share', 'Continue activity', 'Express happiness']))
        scripts.append(EmotionalScript(name='Withdrawal', triggering_emotions=[DETEmotion.DISTRESS, DETEmotion.SADNESS], cognitions=["I've lost something valuable", "I'm helpless", "Things won't improve"], behavioral_responses=['Withdraw', 'Seek comfort', 'Reduce activity', 'Reflect']))
        scripts.append(EmotionalScript(name='Attachment', triggering_emotions=[DETEmotion.LOVE, DETEmotion.TENDERNESS], cognitions=['I care about this', 'This is precious', 'I want to protect this'], behavioral_responses=['Nurture', 'Protect', 'Stay close', 'Express affection']))
        scripts.append(EmotionalScript(name='Orientation', triggering_emotions=[DETEmotion.SURPRISE], cognitions=['This is unexpected', 'What is this?', 'I need to understand'], behavioral_responses=['Stop', 'Orient', 'Pay attention', 'Reassess']))
        scripts.append(EmotionalScript(name='Atonement', triggering_emotions=[DETEmotion.SHAME, DETEmotion.GUILT], cognitions=['I did something wrong', 'I am inadequate', 'I need to make amends'], behavioral_responses=['Apologize', 'Hide', 'Repair damage', 'Self-punishment']))
        return scripts
    def map_core_to_det(self, core_state: np.ndarray) -> np.ndarray:
        det_state = np.zeros(17)
        det_state[DETEmotion.INTEREST] = core_state[CoreEmotion.SEEKING.value] * 0.7
        det_state[DETEmotion.EXCITEMENT] = core_state[CoreEmotion.SEEKING.value] * 0.6
        det_state[DETEmotion.ANGER] = core_state[CoreEmotion.RAGE.value] * 0.8
        det_state[DETEmotion.CONTEMPT] = core_state[CoreEmotion.RAGE.value] * 0.5
        det_state[DETEmotion.DISGUST] = core_state[CoreEmotion.RAGE.value] * 0.6
        det_state[DETEmotion.FEAR] = core_state[CoreEmotion.FEAR.value] * 0.9
        det_state[DETEmotion.SHAME] = core_state[CoreEmotion.FEAR.value] * 0.4
        det_state[DETEmotion.SHYNESS] = core_state[CoreEmotion.FEAR.value] * 0.5
        det_state[DETEmotion.GUILT] = core_state[CoreEmotion.FEAR.value] * 0.3
        det_state[DETEmotion.DESIRE] = core_state[CoreEmotion.LUST.value] * 0.9
        det_state[DETEmotion.LOVE] = core_state[CoreEmotion.CARE.value] * 0.8
        det_state[DETEmotion.TENDERNESS] = core_state[CoreEmotion.CARE.value] * 0.7
        det_state[DETEmotion.DISTRESS] = core_state[CoreEmotion.PANIC_GRIEF.value] * 0.8
        det_state[DETEmotion.SADNESS] = core_state[CoreEmotion.PANIC_GRIEF.value] * 0.7
        det_state[DETEmotion.JOY] = core_state[CoreEmotion.PLAY.value] * 0.8
        det_state[DETEmotion.AMUSEMENT] = core_state[CoreEmotion.PLAY.value] * 0.7
        det_state[DETEmotion.SURPRISE] = core_state[CoreEmotion.PLAY.value] * 0.3
        return det_state
    def map_det_to_core(self, det_state: np.ndarray) -> np.ndarray:
        core_state = np.zeros(7)
        core_state[CoreEmotion.SEEKING.value] = (det_state[DETEmotion.INTEREST] * 0.6 + det_state[DETEmotion.EXCITEMENT] * 0.4) / 1.0
        core_state[CoreEmotion.RAGE.value] = (det_state[DETEmotion.ANGER] * 0.5 + det_state[DETEmotion.CONTEMPT] * 0.3 + det_state[DETEmotion.DISGUST] * 0.2) / 1.0
        core_state[CoreEmotion.FEAR.value] = (det_state[DETEmotion.FEAR] * 0.7 + det_state[DETEmotion.SHAME] * 0.1 + det_state[DETEmotion.SHYNESS] * 0.1 + det_state[DETEmotion.GUILT] * 0.1) / 1.0
        core_state[CoreEmotion.LUST.value] = det_state[DETEmotion.DESIRE]
        core_state[CoreEmotion.CARE.value] = (det_state[DETEmotion.LOVE] * 0.6 + det_state[DETEmotion.TENDERNESS] * 0.4) / 1.0
        core_state[CoreEmotion.PANIC_GRIEF.value] = (det_state[DETEmotion.DISTRESS] * 0.5 + det_state[DETEmotion.SADNESS] * 0.5) / 1.0
        core_state[CoreEmotion.PLAY.value] = (det_state[DETEmotion.JOY] * 0.5 + det_state[DETEmotion.AMUSEMENT] * 0.4 + det_state[DETEmotion.SURPRISE] * 0.1) / 1.0
        return core_state
    def create_det_state_from_core(self, core_state: np.ndarray) -> DETState:
        det_emotions = self.map_core_to_det(core_state)
        return DETState(det_emotions=det_emotions)
    def simulate_appraisal(self, det_state: DETState, time_span: Tuple[float, float]=(0.0, 5.0)) -> DETState:
        if self.use_julia:
            try:
                cognitive_dict = {k: float(v) for k, v in det_state.cognitive_factors.items()}
                final_emotions, final_cognitive = self.det_julia.simulate_appraisal(det_state.det_emotions.tolist(), cognitive_dict, time_span)
                result = DETState(det_emotions=np.array(final_emotions), transition_matrix=det_state.transition_matrix.copy(), cognitive_factors={k: float(v) for k, v in final_cognitive.items()}, active_scripts=det_state.active_scripts.copy(), regulation_capacity=det_state.regulation_capacity, script_history=det_state.script_history.copy())
                return result
            except Exception as e:
                self.logger.error(f'Julia appraisal simulation failed: {e}. Falling back to Python.')
                return self._simulate_appraisal_python(det_state, time_span)
        else:
            return self._simulate_appraisal_python(det_state, time_span)
    def _simulate_appraisal_python(self, det_state: DETState, time_span: Tuple[float, float]) -> DETState:
        emotions = det_state.det_emotions.copy()
        cog_factors = det_state.cognitive_factors.copy()
        t_start, t_end = time_span
        dt = 0.1
        t = t_start
        while t < t_end:
            emotion_derivatives = np.zeros(17)
            for i in range(17):
                decay = -0.2 * emotions[i]
                cognitive_influence = 0.0
                if i in [0, 1, 9, 10, 11, 14, 15, 16]:
                    cognitive_influence += 0.3 * cog_factors['valence']
                else:
                    cognitive_influence += -0.3 * cog_factors['valence']
                cognitive_influence += 0.2 * cog_factors['arousal'] * emotions[i]
                if i in [5, 6, 7, 8]:
                    cognitive_influence += -0.3 * cog_factors['control']
                regulation = -0.2 * emotions[i] * cog_factors['attention']
                emotion_derivatives[i] = decay + cognitive_influence + regulation
            cog_derivatives = {}
            pos_emotions = sum((emotions[i] for i in [0, 1, 9, 10, 11, 14, 15, 16]))
            neg_emotions = sum((emotions[i] for i in [2, 3, 4, 5, 6, 7, 8, 12, 13]))
            cog_derivatives['valence'] = 0.1 * (pos_emotions - neg_emotions) - 0.1 * cog_factors['valence']
            high_arousal = sum((emotions[i] for i in [1, 2, 5, 9, 14, 16]))
            low_arousal = sum((emotions[i] for i in [7, 13]))
            cog_derivatives['arousal'] = 0.2 * (high_arousal - low_arousal) - 0.1 * cog_factors['arousal']
            control_pos = sum((emotions[i] for i in [0, 2, 3]))
            control_neg = sum((emotions[i] for i in [5, 6, 7, 8, 12, 13]))
            cog_derivatives['control'] = 0.1 * (control_pos - control_neg) - 0.1 * cog_factors['control']
            cog_derivatives['certainty'] = -0.3 * emotions[16] - 0.2 * emotions[5] - 0.1 * cog_factors['certainty']
            cog_derivatives['effort'] = 0.2 * (emotions[0] + emotions[1]) + 0.1 * emotions[2] - 0.1 * cog_factors['effort']
            cog_derivatives['attention'] = 0.2 * (emotions[0] + emotions[16]) - 0.1 * cog_factors['attention']
            emotions += emotion_derivatives * dt
            for factor in cog_factors:
                cog_factors[factor] += cog_derivatives[factor] * dt
            emotions = np.clip(emotions, 0.0, 1.0)
            for factor in cog_factors:
                if factor == 'valence':
                    cog_factors[factor] = max(-1.0, min(1.0, cog_factors[factor]))
                else:
                    cog_factors[factor] = max(0.0, min(1.0, cog_factors[factor]))
            t += dt
        result = DETState(det_emotions=emotions, transition_matrix=det_state.transition_matrix.copy(), cognitive_factors=cog_factors, active_scripts=det_state.active_scripts.copy(), regulation_capacity=det_state.regulation_capacity, script_history=det_state.script_history.copy())
        return result
    def regulate_emotion(self, det_state: DETState, target_emotion: DETEmotion, regulation_type: str='reappraisal', time_span: Tuple[float, float]=(0.0, 5.0)) -> DETState:
        if self.use_julia:
            try:
                regulated_emotions = self.det_julia.simulate_regulation(det_state.det_emotions.tolist(), det_state.regulation_capacity, int(target_emotion), regulation_type, time_span)
                result = DETState(det_emotions=np.array(regulated_emotions), transition_matrix=det_state.transition_matrix.copy(), cognitive_factors=det_state.cognitive_factors.copy(), active_scripts=det_state.active_scripts.copy(), regulation_capacity=det_state.regulation_capacity, script_history=det_state.script_history.copy())
                return result
            except Exception as e:
                self.logger.error(f'Julia emotion regulation failed: {e}. Falling back to Python.')
                return self._regulate_emotion_python(det_state, target_emotion, regulation_type, time_span)
        else:
            return self._regulate_emotion_python(det_state, target_emotion, regulation_type, time_span)
    def _regulate_emotion_python(self, det_state: DETState, target_emotion: DETEmotion, regulation_type: str, time_span: Tuple[float, float]) -> DETState:
        emotions = det_state.det_emotions.copy()
        reg_capacity = det_state.regulation_capacity
        target = int(target_emotion)
        t_start, t_end = time_span
        dt = 0.1
        t = t_start
        while t < t_end:
            derivatives = np.zeros(17)
            for i in range(17):
                decay = -0.1 * emotions[i]
                regulation = 0.0
                if regulation_type == 'suppression':
                    if i == target:
                        regulation = -0.3 * reg_capacity
                    else:
                        regulation = 0.05 * reg_capacity
                elif regulation_type == 'reappraisal':
                    if i in [2, 3, 4, 5, 6, 7, 8, 12, 13]:
                        regulation = -0.2 * reg_capacity
                    elif i in [0, 1, 9, 10, 11, 14, 15]:
                        regulation = 0.1 * reg_capacity
                elif regulation_type == 'distraction':
                    regulation = -0.1 * reg_capacity
                derivatives[i] = decay + regulation
            emotions += derivatives * dt
            emotions = np.clip(emotions, 0.0, 1.0)
            t += dt
        result = DETState(det_emotions=emotions, transition_matrix=det_state.transition_matrix.copy(), cognitive_factors=det_state.cognitive_factors.copy(), active_scripts=det_state.active_scripts.copy(), regulation_capacity=det_state.regulation_capacity, script_history=det_state.script_history.copy())
        return result
    def identify_active_scripts(self, det_state: DETState, threshold: float=0.5) -> List[EmotionalScript]:
        active_emotions = [DETEmotion(i) for i, intensity in enumerate(det_state.det_emotions) if intensity > 0.2]
        active_scripts = []
        for script in self.script_library:
            if script.matches_emotions(active_emotions):
                active_scripts.append(script)
        det_state.active_scripts = active_scripts
        if active_scripts:
            det_state.script_history.appendleft(active_scripts[0])
        return active_scripts
    def extract_behavioral_responses(self, det_state: DETState) -> List[str]:
        responses = []
        for script in det_state.active_scripts:
            responses.extend(script.behavioral_responses)
        unique_responses = []
        for response in responses:
            if response not in unique_responses:
                unique_responses.append(response)
        return unique_responses
    def content_to_det_emotion(self, content: str) -> np.ndarray:
        core_emotions = self.base_dynamics.content_to_emotion(content)
        det_emotions = self.map_core_to_det(core_emotions)
        emotion_keywords = {DETEmotion.INTEREST: ['interest', 'curious', 'attention', 'focus'], DETEmotion.EXCITEMENT: ['excite', 'thrill', 'enthusiastic', 'eager'], DETEmotion.ANGER: ['anger', 'mad', 'furious', 'irritated'], DETEmotion.CONTEMPT: ['contempt', 'disdain', 'scorn', 'dismissive'], DETEmotion.DISGUST: ['disgust', 'repulsed', 'revolting', 'gross'], DETEmotion.FEAR: ['fear', 'afraid', 'scared', 'terrified'], DETEmotion.SHAME: ['shame', 'embarrass', 'humiliated', 'inadequate'], DETEmotion.SHYNESS: ['shy', 'timid', 'bashful', 'hesitant'], DETEmotion.GUILT: ['guilt', 'remorse', 'regret', 'apologetic'], DETEmotion.DESIRE: ['desire', 'want', 'crave', 'wish'], DETEmotion.LOVE: ['love', 'adore', 'cherish', 'devoted'], DETEmotion.TENDERNESS: ['tender', 'gentle', 'soft', 'affectionate'], DETEmotion.DISTRESS: ['distress', 'upset', 'troubled', 'worried'], DETEmotion.SADNESS: ['sad', 'sorrow', 'misery', 'unhappy'], DETEmotion.JOY: ['joy', 'happy', 'pleased', 'delight'], DETEmotion.AMUSEMENT: ['amuse', 'laugh', 'funny', 'playful'], DETEmotion.SURPRISE: ['surprise', 'astonish', 'shock', 'unexpected']}
        content_lower = content.lower()
        total_matches = 0
        match_counts = np.zeros(17)
        for emotion, keywords in emotion_keywords.items():
            for keyword in keywords:
                count = content_lower.count(keyword)
                match_counts[emotion] += count
                total_matches += count
        if total_matches > 0:
            normalized_counts = match_counts / (total_matches * 2)
            det_emotions = 0.7 * det_emotions + 0.3 * normalized_counts
            det_emotions = np.clip(det_emotions, 0.1, 1.0)
        return det_emotions