import survey_simulation
ss = survey_simulation.SurveySimulation(mode='test', save_loc='./playback')
ss.new_action('move',[40.0,60.0])
ss.new_action('move',[50.0,60.0])
ss.new_action('move',[60.0,60.0])
ss.new_action('move',[70.0,60.0])
ss.new_action('move',[80.0,60.0])
ss.new_action('move',[90.0,60.0])
ss.new_action('move',[100.0,60.0])
ss.new_action('move',[110.0,60.0])
ss.new_action('move',[120.0,60.0])
ss.new_action('move',[130.0,60.0])
ss.new_action('move',[140.0,60.0])
ss.new_action('move',[150.0,60.0])
ss.new_action('move',[160.0,60.0])
ss.new_action('move',[170.0,60.0])
ss.new_action('move',[180.0,60.0])
ss.new_action('move',[180.0,70.0])
ss.new_action('move',[180.0,80.0])
ss.new_action('move',[190.0,90.0])
ss.new_action('move',[200.0,90.0])
ss.new_action('move',[210.0,90.0])
ss.new_action('move',[220.0,90.0])
ss.new_action('move',[230.0,90.0])
ss.new_action('move',[230.0,80.0])
ss.new_action('move',[240.0,80.0])
ss.save_episode()
ss_pb = survey_simulation.SurveySimulation('playback',save_loc='./playback/Episode2')
