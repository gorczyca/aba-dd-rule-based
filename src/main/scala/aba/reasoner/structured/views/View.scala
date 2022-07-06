package aba.reasoner.structured.views

import interface.ProgramState

trait View {
  def goalView(implicit state: ProgramState): ProgramState
}
