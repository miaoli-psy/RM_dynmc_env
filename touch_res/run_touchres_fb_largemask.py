import os
import random
import math
import csv
import numpy as np

from psychopy import prefs, plugins
plugins.activatePlugins()
prefs.hardware['audioLib'] = 'ptb'
prefs.hardware['audioLatencyMode'] = '3'

from psychopy import visual, core, data, gui, logging, monitors, event
from psychopy.hardware import keyboard

# =========================================================
# BEFORE EXPERIMENT
# =========================================================
TEST_MODE = True
USE_RANDOM_LINE_WIDTH = True  # if false, set SINGLE_LINE_WIDTH_DEG

N_PRACTICE_TRIALS = 4

# ---------------------------------------------------------
# Colors
# ---------------------------------------------------------
BACKGROUND_COLOR = [0, 0, 0]
STIMULUS_COLOR = "white"
FIXATION_COLOR = "white"
TEXT_COLOR = "white"

# ---------------------------------------------------------
# Display / monitor settings
# ---------------------------------------------------------
FULLSCREEN = True
SCREEN_INDEX = 1
RESOLUTION = (2560, 1440)
BG_COLOR = BACKGROUND_COLOR
UNITS = "deg"

# Runtime-only monitor: does NOT depend on Monitor Center
MONITOR_NAME = "runtime_temp_monitor"
MONITOR_WIDTH_CM = 53.0
MONITOR_DISTANCE_CM = 57.0

# ---------------------------------------------------------
# Timing
# ---------------------------------------------------------
FIXATION_OPTIONS_SEC = [1.0, 1.25, 1.5, 1.75, 2.0]
STIM_DURATION_SEC = 0.150
POST_STIMULUS_MASK_GAP_SEC = 0.500
WARNING_DURATION_SEC = 1.0

# ---------------------------------------------------------
# Stimulus geometry (all in deg)
# ---------------------------------------------------------
INNERMOST_ECC_DEG = 8.0
LINE_LENGTH_DEG = 1.2

SINGLE_LINE_WIDTH_DEG = 0.035
LINE_WIDTH_RANGE_DEG = (0.035, 0.25)

EDGE_TO_EDGE_SPACING_RANGE_DEG = (1.0, 1.5)

N_LINES_VALUES = [3, 4]
HEMIFIELDS = ["left", "right"]

# ---------------------------------------------------------
# Noise mask settings
# ---------------------------------------------------------
# ---------------------------------------------------------
# Noise mask settings
# ---------------------------------------------------------
SHOW_MASK_DURING_RESPONSE = True
MASK_RES_PIX = 256
MASK_CONTRAST = 1.0

# Mask size control
# The mask is centered on the line array.
# Option 1: "stimulus_bounds" = mask covers the line array plus margins.
# Option 2: "fixed_size" = mask has a fixed width and height in deg.
MASK_SIZE_MODE = "fixed_size"   # "stimulus_bounds" or "fixed_size"

# Used when MASK_SIZE_MODE = "stimulus_bounds"
# These values enlarge the mask beyond the line array.
MASK_MARGIN_X_DEG = 0.0
MASK_MARGIN_Y_DEG = 0.0

# Used when MASK_SIZE_MODE = "fixed_size"
MASK_FIXED_WIDTH_DEG = 10
MASK_FIXED_HEIGHT_DEG = 10
# ---------------------------------------------------------
# Touch feedback settings
# ---------------------------------------------------------
SHOW_TOUCH_FEEDBACK = True
TOUCH_FEEDBACK_MODE = "ripple"   # "dot" or "ripple"

TOUCH_FEEDBACK_DURATION_SEC = 0.40
TOUCH_FEEDBACK_RADIUS_DEG = 0.12
TOUCH_FEEDBACK_COLOR = "white"

# Ripple feedback parameters
RIPPLE_N_RINGS = 3
RIPPLE_MIN_RADIUS_DEG = 0.05
RIPPLE_MAX_RADIUS_DEG = 0.90
RIPPLE_RING_LIFETIME_SEC = 0.22
RIPPLE_RING_STAGGER_SEC = 0.08
RIPPLE_LINE_WIDTH_PIX = 2.0
RIPPLE_EDGES = 128

# ---------------------------------------------------------
# Block / task structure
# ---------------------------------------------------------
BASE_TASKS = [
    {"name": "innermost", "prompt": "Touch the INNERMOST line."},
    {"name": "second_innermost", "prompt": "Touch the SECOND line from the inside."},
    {"name": "outermost", "prompt": "Touch the OUTERMOST line."},
    {"name": "second_outermost", "prompt": "Touch the SECOND line from the outside."},
]

if TEST_MODE:
    TRIALS_PER_BLOCK = 3
else:
    TRIALS_PER_BLOCK = 50


# ---------------------------------------------------------
# Response settings
# ---------------------------------------------------------
MAX_RESPONSE_TIME_SEC = None
SHOW_RESPONSE_FEEDBACK = False
TOUCH_MOVE_THRESHOLD_DEG = 0.05

# ---------------------------------------------------------
# Mouse visibility
# ---------------------------------------------------------
SHOW_MOUSE_CURSOR = False   # True = show cursor during mouse testing

# ---------------------------------------------------------
# Allowed touch region
# ---------------------------------------------------------
TOUCH_MARGIN_X_DEG = 2.5
TOUCH_MARGIN_Y_DEG = 5.0

DRAW_TOUCH_REGION = True
TOUCH_REGION_LINE_COLOR = "grey"
TOUCH_REGION_LINE_WIDTH = 2.0

# ---------------------------------------------------------
# Instruction continue bar
# ---------------------------------------------------------
INSTR_BAR_WIDTH_DEG = 12.0
INSTR_BAR_HEIGHT_DEG = 1.2
INSTR_BAR_POS_DEG = (0, -6.0)
INSTR_BAR_FILL_COLOR = "dodgerblue"
INSTR_BAR_LINE_COLOR = "deepskyblue"
INSTR_BAR_TEXT_COLOR = "white"

# ---------------------------------------------------------
# Text / controls
# ---------------------------------------------------------
QUIT_KEY = "q"
ESC_KEY = "escape"

# ---------------------------------------------------------
# Data path
# ---------------------------------------------------------
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
DATA_DIR = os.path.join(_thisDir, "data")
os.makedirs(DATA_DIR, exist_ok=True)

# =========================================================
# EXPERIMENT INFO
# =========================================================
EXP_NAME = "RM_touch_4tasks"
PSYCHOPY_VERSION = "2023.1.1"

# =========================================================
# HELPERS
# =========================================================
def choose_fixation_duration_sec():
    return random.choice(FIXATION_OPTIONS_SEC)


def sample_line_width_deg():
    if USE_RANDOM_LINE_WIDTH:
        return random.uniform(LINE_WIDTH_RANGE_DEG[0], LINE_WIDTH_RANGE_DEG[1])
    else:
        return SINGLE_LINE_WIDTH_DEG


def check_quit(default_keyboard):
    keys = default_keyboard.getKeys(keyList=[ESC_KEY], waitRelease=False)
    if keys:
        core.quit()


def build_block_trials(n_trials):
    trials = []
    for _ in range(n_trials):
        trials.append({
            "hemifield": random.choice(HEMIFIELDS),
            "n_lines": random.choice(N_LINES_VALUES),
            "edge_spacing_deg": random.uniform(
                EDGE_TO_EDGE_SPACING_RANGE_DEG[0],
                EDGE_TO_EDGE_SPACING_RANGE_DEG[1]
            ),
            "line_width_deg": sample_line_width_deg()
        })
    return trials


def build_practice_trials():
    return build_block_trials(N_PRACTICE_TRIALS)


def make_fixation(win):
    outer_circle = visual.Circle(
        win=win,
        units="deg",
        radius=0.20,
        edges=64,
        lineColor=FIXATION_COLOR,
        fillColor=None,
        lineWidth=1.5
    )

    inner_circle = visual.Circle(
        win=win,
        units="deg",
        radius=0.05,
        edges=64,
        lineColor=FIXATION_COLOR,
        fillColor=FIXATION_COLOR,
        lineWidth=1.5
    )

    return [outer_circle, inner_circle]


def draw_fixation(fixation):
    for stim in fixation:
        stim.draw()


def make_line_stim(win):
    return visual.Rect(
        win=win,
        units="deg",
        width=SINGLE_LINE_WIDTH_DEG,
        height=LINE_LENGTH_DEG,
        ori=0,
        pos=(0, 0),
        lineWidth=1.0,
        lineColor=STIMULUS_COLOR,
        fillColor=STIMULUS_COLOR
    )


def compute_line_positions(n_lines, hemifield, edge_spacing_deg,
                           line_width_deg,
                           innermost_ecc_deg=INNERMOST_ECC_DEG):
    center_spacing = line_width_deg + edge_spacing_deg

    if hemifield == "right":
        xs = [innermost_ecc_deg + i * center_spacing for i in range(n_lines)]
    else:
        xs = [-(innermost_ecc_deg + i * center_spacing) for i in range(n_lines)]

    ys = [0.0] * n_lines
    return list(zip(xs, ys))


def get_target_position_from_rule(line_positions, target_rule):
    n = len(line_positions)

    if target_rule == "innermost":
        idx = 0
    elif target_rule == "second_innermost":
        idx = 1
    elif target_rule == "outermost":
        idx = n - 1
    elif target_rule == "second_outermost":
        idx = n - 2
    else:
        raise ValueError(f"Unknown target_rule: {target_rule}")

    return line_positions[idx], idx + 1  # 1-based index from innermost


def setup_lines(line_stims, line_positions, line_width_deg, line_length_deg):
    for i, stim in enumerate(line_stims):
        if i < len(line_positions):
            stim.pos = line_positions[i]
            stim.size = (line_width_deg, line_length_deg)
            stim.ori = 0
            stim.opacity = 1.0
        else:
            stim.opacity = 0.0


def compute_touch_bounds(line_positions, line_width_deg, line_length_deg,
                         margin_x_deg=TOUCH_MARGIN_X_DEG,
                         margin_y_deg=TOUCH_MARGIN_Y_DEG):
    xs = [p[0] for p in line_positions]
    ys = [p[1] for p in line_positions]

    left = min(xs) - (line_width_deg / 2.0) - margin_x_deg
    right = max(xs) + (line_width_deg / 2.0) + margin_x_deg
    bottom = min(ys) - (line_length_deg / 2.0) - margin_y_deg
    top = max(ys) + (line_length_deg / 2.0) + margin_y_deg

    return left, right, bottom, top


def compute_stimulus_bounds(line_positions, line_width_deg, line_length_deg,
                            margin_x_deg=MASK_MARGIN_X_DEG,
                            margin_y_deg=MASK_MARGIN_Y_DEG):
    xs = [p[0] for p in line_positions]
    ys = [p[1] for p in line_positions]

    left = min(xs) - (line_width_deg / 2.0) - margin_x_deg
    right = max(xs) + (line_width_deg / 2.0) + margin_x_deg
    bottom = min(ys) - (line_length_deg / 2.0) - margin_y_deg
    top = max(ys) + (line_length_deg / 2.0) + margin_y_deg

    return left, right, bottom, top


def touch_is_within_bounds(x, y, bounds):
    left, right, bottom, top = bounds
    return (left <= x <= right) and (bottom <= y <= top)


def update_touch_region_rect(rect_stim, bounds):
    left, right, bottom, top = bounds
    rect_stim.pos = ((left + right) / 2.0, (bottom + top) / 2.0)
    rect_stim.size = (right - left, top - bottom)


def wait_with_fixation(win, fixation, duration_sec, default_keyboard):
    timer = core.Clock()
    while timer.getTime() < duration_sec:
        check_quit(default_keyboard)
        draw_fixation(fixation)
        win.flip()


def present_stimulus(win, fixation, line_stims, stim_duration_sec, default_keyboard):
    timer = core.Clock()
    while timer.getTime() < stim_duration_sec:
        check_quit(default_keyboard)
        for stim in line_stims:
            if stim.opacity > 0:
                stim.draw()
        draw_fixation(fixation)
        win.flip()


def show_text_until_touch_bar(win, text_stim, bar_stim, bar_label, mouse_obj, default_keyboard):
    mouse_obj.clickReset()
    mouse_obj.setVisible(SHOW_MOUSE_CURSOR)
    last_pos = mouse_obj.getPos()

    while True:
        check_quit(default_keyboard)

        text_stim.draw()
        bar_stim.draw()
        bar_label.draw()
        win.flip()

        current_pos = mouse_obj.getPos()
        dx = current_pos[0] - last_pos[0]
        dy = current_pos[1] - last_pos[1]
        moved_dist = math.sqrt(dx**2 + dy**2)
        pressed = mouse_obj.getPressed()[0]

        if (moved_dist > TOUCH_MOVE_THRESHOLD_DEG) or pressed:
            while mouse_obj.getPressed()[0]:
                check_quit(default_keyboard)
                text_stim.draw()
                bar_stim.draw()
                bar_label.draw()
                win.flip()

            if bar_stim.contains(mouse_obj):
                x, y = mouse_obj.getPos()
                return x, y
            else:
                last_pos = mouse_obj.getPos()
                mouse_obj.clickReset()


def collect_touch_response(win, fixation, mouse_obj, prompt_stim, warning_stim,
                           valid_bounds, default_keyboard, max_wait_sec=None,
                           draw_touch_region=False, touch_region_rect=None,
                           move_threshold_deg=TOUCH_MOVE_THRESHOLD_DEG,
                           mask_stim=None):
    mouse_obj.clickReset()
    mouse_obj.setVisible(SHOW_MOUSE_CURSOR)
    last_pos = mouse_obj.getPos()
    timer = core.Clock()

    while True:
        check_quit(default_keyboard)

        if mask_stim is not None:
            mask_stim.draw()
        draw_fixation(fixation)
        if prompt_stim is not None:
            prompt_stim.draw()
        if draw_touch_region and touch_region_rect is not None:
            touch_region_rect.draw()
        win.flip()

        current_pos = mouse_obj.getPos()
        dx = current_pos[0] - last_pos[0]
        dy = current_pos[1] - last_pos[1]
        moved_dist = math.sqrt(dx**2 + dy**2)
        pressed = mouse_obj.getPressed()[0]

        if (moved_dist > move_threshold_deg) or pressed:
            x, y = current_pos
            rt = timer.getTime()

            while mouse_obj.getPressed()[0]:
                check_quit(default_keyboard)
                if mask_stim is not None:
                    mask_stim.draw()
                draw_fixation(fixation)
                if prompt_stim is not None:
                    prompt_stim.draw()
                if draw_touch_region and touch_region_rect is not None:
                    touch_region_rect.draw()
                win.flip()

            if touch_is_within_bounds(x, y, valid_bounds):
                return x, y, rt, True
            else:
                warn_timer = core.Clock()
                while warn_timer.getTime() < WARNING_DURATION_SEC:
                    check_quit(default_keyboard)
                    if mask_stim is not None:
                        mask_stim.draw()
                    draw_fixation(fixation)
                    if prompt_stim is not None:
                        prompt_stim.draw()
                    if draw_touch_region and touch_region_rect is not None:
                        touch_region_rect.draw()
                    if warning_stim is not None:
                        warning_stim.draw()
                    win.flip()

                last_pos = mouse_obj.getPos()
                mouse_obj.clickReset()
                continue

        if (max_wait_sec is not None) and (timer.getTime() >= max_wait_sec):
            return None, None, None, False


def compute_error_metrics(touch_x, touch_y, target_x, target_y):
    if touch_x is None or touch_y is None:
        return None, None, None

    dx = touch_x - target_x
    dy = touch_y - target_y
    euclid = math.sqrt(dx**2 + dy**2)
    return dx, dy, euclid


def show_feedback(win, fixation, feedback_stim, msg, duration_sec, default_keyboard):
    feedback_stim.text = msg
    timer = core.Clock()
    while timer.getTime() < duration_sec:
        check_quit(default_keyboard)
        draw_fixation(fixation)
        feedback_stim.draw()
        win.flip()


def show_touch_feedback(win, fixation, mask_stim, prompt_stim,
                        touch_region_rect, draw_touch_region,
                        feedback_dot, touch_x, touch_y,
                        duration_sec, default_keyboard):
    feedback_dot.pos = (touch_x, touch_y)

    timer = core.Clock()
    while timer.getTime() < duration_sec:
        check_quit(default_keyboard)

        if mask_stim is not None:
            mask_stim.draw()
        draw_fixation(fixation)

        if prompt_stim is not None:
            prompt_stim.draw()

        if draw_touch_region and touch_region_rect is not None:
            touch_region_rect.draw()

        feedback_dot.draw()
        win.flip()


def save_trial_csv_row(csv_writer, row_dict):
    csv_writer.writerow(row_dict)


def make_noise_mask_stim(win):
    noise_img = np.random.uniform(-1, 1, (MASK_RES_PIX, MASK_RES_PIX))
    return visual.ImageStim(
        win=win,
        image=noise_img,
        units="deg",
        pos=(0, 0),
        size=(1.0, 1.0),
        color=[1, 1, 1],
        colorSpace="rgb",
        opacity=MASK_CONTRAST,
        interpolate=True
    )


def refresh_noise_mask(mask_stim):
    noise_img = np.random.uniform(-1, 1, (MASK_RES_PIX, MASK_RES_PIX))
    mask_stim.image = noise_img


def update_mask_from_bounds(mask_stim, bounds):
    left, right, bottom, top = bounds
    mask_stim.pos = ((left + right) / 2.0, (bottom + top) / 2.0)
    mask_stim.size = (right - left, top - bottom)

def update_mask_size(mask_stim, stim_bounds):
    """
    Update the mask position and size.

    If MASK_SIZE_MODE = "stimulus_bounds", the mask covers the stimulus array
    plus MASK_MARGIN_X_DEG and MASK_MARGIN_Y_DEG.

    If MASK_SIZE_MODE = "fixed_size", the mask is centered on the stimulus array
    but has a fixed width and height.
    """
    left, right, bottom, top = stim_bounds
    center_x = (left + right) / 2.0
    center_y = (bottom + top) / 2.0

    if MASK_SIZE_MODE == "stimulus_bounds":
        mask_stim.pos = (center_x, center_y)
        mask_stim.size = (right - left, top - bottom)

    elif MASK_SIZE_MODE == "fixed_size":
        mask_stim.pos = (center_x, center_y)
        mask_stim.size = (MASK_FIXED_WIDTH_DEG, MASK_FIXED_HEIGHT_DEG)

    else:
        raise ValueError(
            f"Unknown MASK_SIZE_MODE: {MASK_SIZE_MODE}. "
            "Use 'stimulus_bounds' or 'fixed_size'."
        )
    
def present_mask(win, fixation, mask_stim, duration_sec, default_keyboard):
    timer = core.Clock()
    while timer.getTime() < duration_sec:
        check_quit(default_keyboard)
        mask_stim.draw()
        draw_fixation(fixation)
        win.flip()

def build_random_block_order(base_tasks):
    first_half = base_tasks.copy()
    second_half = base_tasks.copy()

    random.shuffle(first_half)
    random.shuffle(second_half)

    while second_half[0]["name"] == first_half[-1]["name"]:
        random.shuffle(second_half)

    return first_half + second_half

def make_touch_feedback_rings(win):
    rings = []
    for _ in range(RIPPLE_N_RINGS):
        ring = visual.Circle(
            win=win,
            units="deg",
            radius=RIPPLE_MIN_RADIUS_DEG,
            edges=RIPPLE_EDGES,
            lineColor=TOUCH_FEEDBACK_COLOR,
            fillColor=None,
            lineWidth=RIPPLE_LINE_WIDTH_PIX,
            opacity=0.0,
            pos=(0, 0)
        )
        rings.append(ring)
    return rings


def show_ripple_touch_feedback(win, fixation, mask_stim, prompt_stim,
                               touch_region_rect, draw_touch_region,
                               feedback_rings, touch_x, touch_y,
                               duration_sec, default_keyboard):
    timer = core.Clock()

    while timer.getTime() < duration_sec:
        check_quit(default_keyboard)

        if mask_stim is not None:
            mask_stim.draw()

        draw_fixation(fixation)

        if prompt_stim is not None:
            prompt_stim.draw()

        if draw_touch_region and touch_region_rect is not None:
            touch_region_rect.draw()

        t = timer.getTime()

        for ring_index, ring in enumerate(feedback_rings):
            local_t = t - (ring_index * RIPPLE_RING_STAGGER_SEC)

            if 0.0 <= local_t <= RIPPLE_RING_LIFETIME_SEC:
                progress = local_t / RIPPLE_RING_LIFETIME_SEC
                ring.pos = (touch_x, touch_y)
                ring.radius = (
                    RIPPLE_MIN_RADIUS_DEG
                    + progress * (RIPPLE_MAX_RADIUS_DEG - RIPPLE_MIN_RADIUS_DEG)
                )
                ring.opacity = max(0.0, 1.0 - progress)
                ring.lineWidth = max(0.5, RIPPLE_LINE_WIDTH_PIX * (1.0 - 0.5 * progress))
                ring.draw()

        win.flip()
# =========================================================
# GUI
# =========================================================
expInfo = {
    "participant": str(random.randint(1000000, 9999999)),
    "age": "",
    "gender": ["female", "male", "other"],
}

dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=EXP_NAME)
if not dlg.OK:
    core.quit()

expInfo["date"] = data.getDateStr()
expInfo["expName"] = EXP_NAME
expInfo["psychopyVersion"] = PSYCHOPY_VERSION

filename_base = os.path.join(DATA_DIR, f'{expInfo["participant"]}_{EXP_NAME}_{expInfo["date"]}')


# =========================================================
# LOGGING / EXP HANDLER
# =========================================================
thisExp = data.ExperimentHandler(
    name=EXP_NAME,
    version="",
    extraInfo=expInfo,
    runtimeInfo=None,
    originPath=__file__,
    savePickle=True,
    saveWideText=True,
    dataFileName=filename_base
)

logFile = logging.LogFile(filename_base + ".log", level=logging.EXP)
logging.console.setLevel(logging.WARNING)


# =========================================================
# WINDOW / MONITOR / INPUT
# =========================================================
monitor_obj = monitors.Monitor(
    name=MONITOR_NAME,
    width=MONITOR_WIDTH_CM,
    distance=MONITOR_DISTANCE_CM
)
monitor_obj.setSizePix(RESOLUTION)

win = visual.Window(
    size=RESOLUTION,
    fullscr=FULLSCREEN,
    screen=SCREEN_INDEX,
    winType="pyglet",
    allowStencil=False,
    monitor=monitor_obj,
    color=BG_COLOR,
    colorSpace="rgb",
    blendMode='avg',
    useFBO=True,
    units=UNITS,
    checkTiming=False
)
win.mouseVisible = SHOW_MOUSE_CURSOR

defaultKeyboard = keyboard.Keyboard(backend="ptb")
kb_end = keyboard.Keyboard(backend="ptb")
mouse = event.Mouse(win=win, visible=False)


# =========================================================
# STIMULI
# =========================================================
fixation = make_fixation(win)

max_n_lines = max(N_LINES_VALUES)
line_stims = [make_line_stim(win) for _ in range(max_n_lines)]
noise_mask_stim = make_noise_mask_stim(win)

touch_feedback_dot = visual.Circle(
    win=win,
    units="deg",
    radius=TOUCH_FEEDBACK_RADIUS_DEG,
    edges=64,
    lineColor=TOUCH_FEEDBACK_COLOR,
    fillColor=TOUCH_FEEDBACK_COLOR,
    lineWidth=1.0,
    opacity=1.0,
    pos=(0, 0)
)

touch_feedback_rings = make_touch_feedback_rings(win)

instruction_text = visual.TextStim(
    win=win,
    text=(
        "Please keep your eyes on the central fixation.\n\n"
        "Touch the target line as instructed.\n\n"
        "Touch the blue bar below to start practice."
    ),
    font="Arial",
    units="pix",
    pos=(0, 80),
    height=34,
    wrapWidth=1400,
    color=TEXT_COLOR
)

before_main_text = visual.TextStim(
    win=win,
    text="Practice is finished.\n\nTouch the blue bar below when you are ready for the main experiment.",
    font="Arial",
    units="pix",
    pos=(0, 80),
    height=36,
    wrapWidth=1400,
    color=TEXT_COLOR
)

block_text = visual.TextStim(
    win=win,
    text="",
    font="Arial",
    units="pix",
    pos=(0, 80),
    height=38,
    wrapWidth=1400,
    color=TEXT_COLOR
)

response_prompt = visual.TextStim(
    win=win,
    text="",
    font="Arial",
    units="deg",
    pos=(0, 3.0),
    height=0.6,
    color=TEXT_COLOR
)

warning_text = visual.TextStim(
    win=win,
    text="Please touch closer to the line array.",
    font="Arial",
    units="deg",
    pos=(0, -3.5),
    height=0.6,
    color=TEXT_COLOR
)

feedback_text = visual.TextStim(
    win=win,
    text="",
    font="Arial",
    units="deg",
    pos=(0, -3.0),
    height=0.6,
    color=TEXT_COLOR
)

break_text = visual.TextStim(
    win=win,
    text="You can take a short break.\n\nTouch the blue bar below to continue.",
    font="Arial",
    units="pix",
    pos=(0, 80),
    height=42,
    wrapWidth=1400,
    color=TEXT_COLOR
)

end_text = visual.TextStim(
    win=win,
    text=f"End of experiment.\n\nExperimenter: press '{QUIT_KEY}' to quit.",
    font="Arial",
    units="pix",
    pos=(0, 0),
    height=42,
    wrapWidth=1400,
    color=TEXT_COLOR
)

instruction_continue_bar = visual.Rect(
    win=win,
    units="deg",
    width=INSTR_BAR_WIDTH_DEG,
    height=INSTR_BAR_HEIGHT_DEG,
    pos=INSTR_BAR_POS_DEG,
    lineWidth=2.0,
    lineColor=INSTR_BAR_LINE_COLOR,
    fillColor=INSTR_BAR_FILL_COLOR,
    opacity=1.0
)

instruction_continue_label = visual.TextStim(
    win=win,
    text="Touch here to continue",
    font="Arial",
    units="deg",
    pos=INSTR_BAR_POS_DEG,
    height=0.55,
    color=INSTR_BAR_TEXT_COLOR
)

touch_region_rect = visual.Rect(
    win=win,
    units="deg",
    pos=(0, 0),
    width=1.0,
    height=1.0,
    lineColor=TOUCH_REGION_LINE_COLOR,
    fillColor=None,
    lineWidth=TOUCH_REGION_LINE_WIDTH,
    opacity=1.0
)


# =========================================================
# RANDOMIZE BLOCK ORDER
# =========================================================
block_order = build_random_block_order(BASE_TASKS)
N_BLOCKS = len(block_order)

for i, block_task in enumerate(block_order, start=1):
    thisExp.extraInfo[f"block_{i}_task"] = block_task["name"]


# =========================================================
# CSV SIDECAR
# =========================================================
csv_path = filename_base + "_touch_trials.csv"
csv_fieldnames = [
    "participant",
    "date",
    "phase",
    "block_index",
    "task",
    "trial_index",
    "hemifield",
    "setsize",
    "edge_spacing_deg",
    "line_width_deg",
    "line_length_deg",
    "innermost_ecc_deg",
    "target_index_from_innermost",
    "target_x_deg",
    "target_y_deg",
    "touch_x_deg",
    "touch_y_deg",
    "touch_rt_sec",
    "error_x_deg",
    "error_y_deg",
    "error_euclidean_deg",
    "post_stimulus_mask_gap_sec",
    "touch_accepted"
]

csv_file = open(csv_path, mode="w", newline="", encoding="utf-8")
csv_writer = csv.DictWriter(csv_file, fieldnames=csv_fieldnames)
csv_writer.writeheader()


# =========================================================
# CORE TRIAL FUNCTION
# =========================================================
def run_trial(trial, phase, block_index, block_task, trial_index):
    fixation_dur = choose_fixation_duration_sec()

    hemifield = trial["hemifield"]
    n_lines = int(trial["n_lines"])
    edge_spacing_deg = float(trial["edge_spacing_deg"])
    line_width_deg = float(trial["line_width_deg"])
    task = block_task["name"]

    line_positions = compute_line_positions(
        n_lines=n_lines,
        hemifield=hemifield,
        edge_spacing_deg=edge_spacing_deg,
        line_width_deg=line_width_deg,
        innermost_ecc_deg=INNERMOST_ECC_DEG
    )

    (target_x, target_y), target_index_from_innermost = get_target_position_from_rule(
        line_positions=line_positions,
        target_rule=task
    )

    valid_bounds = compute_touch_bounds(
        line_positions=line_positions,
        line_width_deg=line_width_deg,
        line_length_deg=LINE_LENGTH_DEG
    )

    stim_bounds = compute_stimulus_bounds(
        line_positions=line_positions,
        line_width_deg=line_width_deg,
        line_length_deg=LINE_LENGTH_DEG,
        margin_x_deg=MASK_MARGIN_X_DEG,
        margin_y_deg=MASK_MARGIN_Y_DEG
    )

    update_touch_region_rect(touch_region_rect, valid_bounds)
    refresh_noise_mask(noise_mask_stim)
    update_mask_size(noise_mask_stim, stim_bounds)

    setup_lines(
        line_stims=line_stims,
        line_positions=line_positions,
        line_width_deg=line_width_deg,
        line_length_deg=LINE_LENGTH_DEG
    )

    response_prompt.text = block_task["prompt"]

    wait_with_fixation(
        win=win,
        fixation=fixation,
        duration_sec=fixation_dur,
        default_keyboard=defaultKeyboard
    )

    present_stimulus(
        win=win,
        fixation=fixation,
        line_stims=line_stims,
        stim_duration_sec=STIM_DURATION_SEC,
        default_keyboard=defaultKeyboard
    )

    if POST_STIMULUS_MASK_GAP_SEC > 0:
        present_mask(
            win=win,
            fixation=fixation,
            mask_stim=noise_mask_stim,
            duration_sec=POST_STIMULUS_MASK_GAP_SEC,
            default_keyboard=defaultKeyboard
        )

    mask_for_response = noise_mask_stim if SHOW_MASK_DURING_RESPONSE else None

    touch_x, touch_y, touch_rt, touch_accepted = collect_touch_response(
        win=win,
        fixation=fixation,
        mouse_obj=mouse,
        prompt_stim=None,
        warning_stim=warning_text,
        valid_bounds=valid_bounds,
        default_keyboard=defaultKeyboard,
        max_wait_sec=MAX_RESPONSE_TIME_SEC,
        draw_touch_region=DRAW_TOUCH_REGION,
        touch_region_rect=touch_region_rect,
        mask_stim=mask_for_response
    )

    err_x, err_y, err_euclid = compute_error_metrics(
        touch_x=touch_x,
        touch_y=touch_y,
        target_x=target_x,
        target_y=target_y
    )

    if SHOW_TOUCH_FEEDBACK and touch_accepted and touch_x is not None and touch_y is not None:
        if TOUCH_FEEDBACK_MODE == "ripple":
            show_ripple_touch_feedback(
                win=win,
                fixation=fixation,
                mask_stim=mask_for_response,
                prompt_stim=None,
                touch_region_rect=touch_region_rect,
                draw_touch_region=DRAW_TOUCH_REGION,
                feedback_rings=touch_feedback_rings,
                touch_x=touch_x,
                touch_y=touch_y,
                duration_sec=TOUCH_FEEDBACK_DURATION_SEC,
                default_keyboard=defaultKeyboard
            )
        else:
            show_touch_feedback(
                win=win,
                fixation=fixation,
                mask_stim=mask_for_response,
                prompt_stim=None,
                touch_region_rect=touch_region_rect,
                draw_touch_region=DRAW_TOUCH_REGION,
                feedback_dot=touch_feedback_dot,
                touch_x=touch_x,
                touch_y=touch_y,
                duration_sec=TOUCH_FEEDBACK_DURATION_SEC,
                default_keyboard=defaultKeyboard
            )
    if SHOW_RESPONSE_FEEDBACK and phase == "practice" and err_euclid is not None:
        show_feedback(
            win=win,
            fixation=fixation,
            feedback_stim=feedback_text,
            msg=f"Error = {err_euclid:.2f} deg",
            duration_sec=0.8,
            default_keyboard=defaultKeyboard
        )

    thisExp.addData("phase", phase)
    thisExp.addData("block_index", block_index)
    thisExp.addData("task", task)
    thisExp.addData("trial_index", trial_index)
    thisExp.addData("hemifield", hemifield)
    thisExp.addData("setsize", n_lines)
    thisExp.addData("edge_spacing_deg", edge_spacing_deg)
    thisExp.addData("line_width_deg", line_width_deg)
    thisExp.addData("line_length_deg", LINE_LENGTH_DEG)
    thisExp.addData("innermost_ecc_deg", INNERMOST_ECC_DEG)
    thisExp.addData("target_index_from_innermost", target_index_from_innermost)
    thisExp.addData("target_x_deg", target_x)
    thisExp.addData("target_y_deg", target_y)
    thisExp.addData("touch_x_deg", touch_x)
    thisExp.addData("touch_y_deg", touch_y)
    thisExp.addData("touch_rt_sec", touch_rt)
    thisExp.addData("error_x_deg", err_x)
    thisExp.addData("error_y_deg", err_y)
    thisExp.addData("error_euclidean_deg", err_euclid)
    thisExp.addData("post_stimulus_mask_gap_sec", POST_STIMULUS_MASK_GAP_SEC)
    thisExp.addData("touch_accepted", touch_accepted)
    thisExp.nextEntry()

    save_trial_csv_row(csv_writer, {
        "participant": expInfo["participant"],
        "date": expInfo["date"],
        "phase": phase,
        "block_index": block_index,
        "task": task,
        "trial_index": trial_index,
        "hemifield": hemifield,
        "setsize": n_lines,
        "edge_spacing_deg": edge_spacing_deg,
        "line_width_deg": line_width_deg,
        "line_length_deg": LINE_LENGTH_DEG,
        "innermost_ecc_deg": INNERMOST_ECC_DEG,
        "target_index_from_innermost": target_index_from_innermost,
        "target_x_deg": target_x,
        "target_y_deg": target_y,
        "touch_x_deg": touch_x,
        "touch_y_deg": touch_y,
        "touch_rt_sec": touch_rt,
        "error_x_deg": err_x,
        "error_y_deg": err_y,
        "error_euclidean_deg": err_euclid,
        "post_stimulus_mask_gap_sec": POST_STIMULUS_MASK_GAP_SEC,
        "touch_accepted": touch_accepted
    })


# =========================================================
# RUN
# =========================================================
show_text_until_touch_bar(
    win=win,
    text_stim=instruction_text,
    bar_stim=instruction_continue_bar,
    bar_label=instruction_continue_label,
    mouse_obj=mouse,
    default_keyboard=defaultKeyboard
)

practice_task = block_order[0]
response_prompt.text = practice_task["prompt"]

practice_trials = build_practice_trials()
for i, trial in enumerate(practice_trials, start=1):
    run_trial(
        trial=trial,
        phase="practice",
        block_index=0,
        block_task=practice_task,
        trial_index=i
    )

show_text_until_touch_bar(
    win=win,
    text_stim=before_main_text,
    bar_stim=instruction_continue_bar,
    bar_label=instruction_continue_label,
    mouse_obj=mouse,
    default_keyboard=defaultKeyboard
)

for block_index, block_task in enumerate(block_order, start=1):
    block_text.text = (
        f"Block {block_index} of {N_BLOCKS}\n\n"
        f"In this block, your task is:\n"
        f"{block_task['prompt']}\n\n"
        f"Please keep your eyes on the central fixation mark.\n\n"
        f"Touch the blue bar below to start."
    )
    show_text_until_touch_bar(
        win=win,
        text_stim=block_text,
        bar_stim=instruction_continue_bar,
        bar_label=instruction_continue_label,
        mouse_obj=mouse,
        default_keyboard=defaultKeyboard
    )

    block_trials = build_block_trials(TRIALS_PER_BLOCK)

    for trial_index, trial in enumerate(block_trials, start=1):
        run_trial(
            trial=trial,
            phase="main",
            block_index=block_index,
            block_task=block_task,
            trial_index=trial_index
        )

    if block_index < N_BLOCKS:
        break_text.text = (
            f"Block {block_index} of {N_BLOCKS} completed.\n\n"
            "Touch the blue bar below to continue."
        )
        show_text_until_touch_bar(
            win=win,
            text_stim=break_text,
            bar_stim=instruction_continue_bar,
            bar_label=instruction_continue_label,
            mouse_obj=mouse,
            default_keyboard=defaultKeyboard
        )

kb_end.clearEvents()
kb_end.clock.reset()

while True:
    check_quit(defaultKeyboard)
    end_text.draw()
    win.flip()

    keys = kb_end.getKeys(keyList=[QUIT_KEY], waitRelease=False)
    if keys:
        break


# =========================================================
# SAVE / CLOSE
# =========================================================
csv_file.close()

thisExp.saveAsWideText(filename_base + ".csv", delim="auto")
thisExp.saveAsPickle(filename_base)
logging.flush()

thisExp.abort()
win.close()
core.quit()