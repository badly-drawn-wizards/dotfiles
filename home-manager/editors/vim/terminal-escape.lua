-- Terminal escape with timeout
-- Double-tap <Esc> to exit terminal mode
-- Single <Esc> + another key sends both to terminal
-- Single <Esc> + timeout sends <Esc> to terminal

function _G.terminal_escape()
  local timeout_ms = 300
  local start_time = vim.loop.now()
  local char = nil

  -- Poll for character with timeout
  while vim.loop.now() - start_time < timeout_ms do
    char = vim.fn.getchar(0)
    if char ~= 0 then
      break
    end
    vim.wait(10) -- small wait between polls
  end

  -- Use 'n' flag to avoid triggering mappings (prevents recursion)
  if char == 0 then
    -- Timeout - send single escape
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<Esc>', true, true, true), 'n', false)
  elseif char == 27 then
    -- Double escape - exit terminal mode
    vim.cmd('stopinsert')
  else
    -- Other key - send escape + key
    local key = type(char) == 'number' and vim.fn.nr2char(char) or char
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<Esc>', true, true, true) .. key, 'n', false)
  end
end
