{-# LANGUAGE CPP, CApiFFI, ForeignFunctionInterface #-}

module Dwf.Dll.Wrap where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- System Settings

foreign import capi "dwf.h FDwfGetLastError"
    fdwf_get_last_error :: Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfGetLastErrorMsg" 
    fdwf_get_last_error_msg :: CString -> IO CInt
foreign import capi "dwf.h FDwfGetVersion" 
    fdwf_get_version :: CString -> IO CInt

foreign import capi "dwf.h FDwfParamSet"
    fdwf_param_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfParamGet"
    fdwf_param_get :: CInt -> Ptr CInt -> IO CInt

-- Enumeration

foreign import capi "dwf.h FDwfEnum" 
    fdwf_enum :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfEnumDeviceType"
    fdwf_enum_device_type :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfEnumDeviceIsOpened" 
    fdwf_enum_device_is_opened :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfEnumUserName"
    fdwf_enum_user_name :: CInt -> CString -> IO CInt
foreign import capi "dwf.h FDwfEnumDeviceName" 
    fdwf_enum_device_name :: CInt -> CString -> IO CInt
foreign import capi "dwf.h FDwfEnumSN" 
    fdwf_enum_sn :: CInt -> CString -> IO CInt
foreign import capi "dwf.h FDwfEnumConfig"
    fdwf_enum_config :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfEnumConfigInfo"
    fdwf_enum_config_info :: CInt -> CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceOpen"
    fdwf_device_open :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceConfigOpen"
    fdwf_device_config_open :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceClose"
    fdwf_device_close :: CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceCloseAll"
    fdwf_device_close_all :: IO CInt

foreign import capi "dwf.h FDwfDeviceAutoConfigureSet" 
    fdwf_device_auto_configure_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceAutoConfigureGet" 
    fdwf_device_auto_configure_get :: CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceReset"
    fdwf_device_reset :: CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceEnableSet"
    fdwf_device_enable_set :: CInt -> CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceTriggerInfo"
    fdwf_device_trigger_info :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceTriggerSet"
    fdwf_device_trigger_set :: CInt -> CInt -> CUChar -> IO CInt
foreign import capi "dwf.h FDwfDeviceTriggerGet"
    fdwf_device_trigger_get :: CInt -> CInt -> Ptr CUChar -> IO CInt
foreign import capi "dwf.h FDwfDeviceTriggerSlopeInfo"
    fdwf_device_trigger_slope_info :: CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceTriggerPC"
    fdwf_device_trigger_pc :: CInt -> IO CInt

foreign import capi "dwf.h FDwfDeviceParamSet"
    fdwf_device_param_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfDeviceParamGet"
    fdwf_device_param_get :: CInt -> CInt -> Ptr CInt -> IO CInt

-- Analog Output

foreign import capi "dwf.h FDwfAnalogOutCount"
    fdwf_analog_out_count :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutMasterSet"
    fdwf_analog_out_master_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutMasterGet"
    fdwf_analog_out_master_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutTriggerSourceSet"
    fdwf_analog_out_trigger_source_set :: CInt -> CInt -> CUChar -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutTriggerSourceGet"
    fdwf_analog_out_trigger_source_get :: CInt -> CInt -> Ptr (CUChar) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutTriggerSlopeSet"
    fdwf_analog_out_trigger_slope_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutTriggerSlopeGet"
    fdwf_analog_out_trigger_slope_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutRunInfo"
    fdwf_analog_out_run_info :: CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRunSet"
    fdwf_analog_out_run_set :: CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRunGet"
    fdwf_analog_out_run_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRunStatus"
    fdwf_analog_out_run_status :: CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutWaitInfo"
    fdwf_analog_out_wait_info :: CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutWaitSet"
    fdwf_analog_out_wait_set :: CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutWaitGet"
    fdwf_analog_out_wait_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutRepeatInfo"
    fdwf_analog_out_repeat_info :: CInt -> CInt -> Ptr (CInt) -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRepeatSet"
    fdwf_analog_out_repeat_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRepeatGet"
    fdwf_analog_out_repeat_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRepeatStatus"
    fdwf_analog_out_repeat_status :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutRepeatTriggerSet"
    fdwf_analog_out_repeat_trigger_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutRepeatTriggerGet"
    fdwf_analog_out_repeat_trigger_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutLimitationInfo"
    fdwf_analog_out_limitation_info :: CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutLimitationSet"
    fdwf_analog_out_limitation_set :: CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutLimitationGet"
    fdwf_analog_out_limitation_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutModeSet"
    fdwf_analog_out_mode_set :: CInt -> CInt -> CInt-> IO CInt
foreign import capi "dwf.h FDwfAnalogOutModeGet"
    fdwf_analog_out_mode_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutIdleInfo"
    fdwf_analog_out_idle_info :: CInt -> CInt -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutIdleSet"
    fdwf_analog_out_idle_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutIdleGet"
    fdwf_analog_out_idle_get :: CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeInfo"
    fdwf_analog_out_node_info :: CInt -> CInt -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeEnableSet" 
    fdwf_analog_out_node_enable_set :: CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeEnableGet"
    fdwf_analog_out_node_enable_get :: CInt -> CInt -> CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeFunctionInfo" 
    fdwf_analog_out_node_function_info :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeFunctionSet" 
    fdwf_analog_out_node_function_set :: CInt -> CInt -> CInt -> CUChar -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeFunctionGet" 
    fdwf_analog_out_node_function_get :: CInt -> CInt -> CInt -> Ptr (CUChar) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeFrequencyInfo"
    fdwf_analog_out_node_frequency_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeFrequencySet" 
    fdwf_analog_out_node_frequency_set :: CInt -> CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeFrequencyGet" 
    fdwf_analog_out_node_frequency_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeAmplitudeInfo"
    fdwf_analog_out_node_amplitude_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeAmplitudeSet" 
    fdwf_analog_out_node_amplitude_set :: CInt -> CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeAmplitudeGet" 
    fdwf_analog_out_node_amplitude_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeOffsetInfo"
    fdwf_analog_out_node_offset_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeOffsetSet" 
    fdwf_analog_out_node_offset_set :: CInt -> CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeOffsetGet" 
    fdwf_analog_out_node_offset_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodeSymmetryInfo"
    fdwf_analog_out_node_symmetry_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeSymmetrySet" 
    fdwf_analog_out_node_symmetry_set :: CInt -> CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeSymmetryGet" 
    fdwf_analog_out_node_symmetry_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodePhaseInfo"
    fdwf_analog_out_node_phase_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodePhaseSet" 
    fdwf_analog_out_node_phase_set :: CInt -> CInt -> CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodePhaseGet" 
    fdwf_analog_out_node_phase_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeDataInfo"
    fdwf_analog_out_node_data_info :: CInt -> CInt -> CInt -> Ptr (CInt) -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodeDataSet"
    fdwf_analog_out_node_data_set :: CInt -> CInt -> CInt -> Ptr (CDouble) -> CInt -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutReset" 
    fdwf_analog_out_reset :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutConfigure" 
    fdwf_analog_out_configure :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutStatus" 
    fdwf_analog_out_status :: CInt -> CInt -> Ptr (CUChar) -> IO CInt

foreign import capi "dwf.h FDwfAnalogOutNodePlayStatus"
    fdwf_analog_out_node_play_status :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogOutNodePlayData"
    fdwf_analog_out_node_play_data :: CInt -> CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt

-- Analog In

foreign import capi "dwf.h FDwfAnalogInReset" 
    fdwf_analog_in_reset :: CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInConfigure"
    fdwf_analog_in_configure :: CInt -> CInt -> CInt -> IO CInt 

-- TriggerForce

foreign import capi "dwf.h FDwfAnalogInStatus"
    fdwf_analog_in_status :: CInt -> CInt -> Ptr CUChar -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusSamplesLeft"
    fdwf_analog_in_status_samples_left :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusSamplesValid"
    fdwf_analog_in_status_samples_valid :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusIndexWrite"
    fdwf_analog_in_status_index_write :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusAutoTriggered"
    fdwf_analog_in_status_auto_triggered :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusData"
    fdwf_analog_in_status_data :: CInt -> CInt -> Ptr CDouble -> CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInStatusData2"
    fdwf_analog_in_status_data2 :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInStatusData16"
    fdwf_analog_in_status_data16 :: CInt -> CInt -> Ptr CShort -> CInt -> CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInStatusNoise"
    fdwf_analog_in_status_noise :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInStatusNoise2"
    fdwf_analog_in_status_noise2 :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInStatusSample"
    fdwf_analog_in_status_sample :: CInt -> CInt -> Ptr CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInStatusTime"
    fdwf_analog_in_status_time :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfAnalogInStatusRecord"
    fdwf_analog_in_status_record :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInRecordLengthSet"
    fdwf_analog_in_record_length_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInRecordLengthGet"
    fdwf_analog_in_record_length_get :: CInt -> Ptr CDouble -> IO CInt

foreign import capi "dwf.h FDwfAnalogInFrequencyInfo"
    fdwf_analog_in_frequency_info :: CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInFrequencySet" 
    fdwf_analog_in_frequency_set :: CInt -> CDouble -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInFrequencyGet" 
    fdwf_analog_in_frequency_get :: CInt -> Ptr CDouble -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInBitsInfo"
    fdwf_analog_in_bits_info :: CInt -> Ptr CInt -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInBufferSizeInfo"
    fdwf_analog_in_buffer_size_info :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInBufferSizeGet"
    fdwf_analog_in_buffer_size_get :: CInt -> Ptr CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInBufferSizeSet"
    fdwf_analog_in_buffer_size_set :: CInt -> CInt -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInNoiseSizeInfo"
    fdwf_analog_in_noise_size_info :: CInt -> Ptr CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInNoiseSizeSet"
    fdwf_analog_in_noise_size_set :: CInt -> CInt -> IO CInt 
foreign import capi "dwf.h FDwfAnalogInNoiseSizeGet"
    fdwf_analog_in_noise_size_get :: CInt -> Ptr CInt -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInAcquisitionModeInfo"
    fdwf_analog_in_acquisition_mode_info :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInAcquisitionModeSet" 
    fdwf_analog_in_acquisition_mode_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInAcquisitionModeGet"
    fdwf_analog_in_acquisition_mode_get :: CInt -> Ptr CInt -> IO CInt 

foreign import capi "dwf.h FDwfAnalogInChannelCount" 
    fdwf_analog_in_channel_count :: CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelEnableSet" 
    fdwf_analog_in_channel_enable_set :: CInt -> CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelEnableGet" 
    fdwf_analog_in_channel_enable_get :: CInt -> CInt -> Ptr CInt -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelFilterInfo"
    fdwf_analog_in_channel_filter_info :: CInt -> Ptr CInt-> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelFilterGet"
    fdwf_analog_in_channel_filter_get :: CInt -> CInt -> Ptr CInt-> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelFilterSet"
    fdwf_analog_in_channel_filter_set :: CInt -> CInt -> CInt -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelRangeInfo" 
    fdwf_analog_in_channel_range_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelRangeSteps"
    fdwf_analog_in_channel_range_steps :: CInt -> Ptr (CDouble) -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelRangeGet" 
    fdwf_analog_in_channel_range_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelRangeSet" 
    fdwf_analog_in_channel_range_set :: CInt -> CInt -> CDouble -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelOffsetInfo"
    fdwf_analog_in_channel_offset_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelOffsetGet" 
    fdwf_analog_in_channel_offset_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelOffsetSet" 
    fdwf_analog_in_channel_offset_set :: CInt -> CInt -> CDouble -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelAttenuationGet" 
    fdwf_analog_in_channel_attenuation_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelAttenuationSet" 
    fdwf_analog_in_channel_attenuation_set :: CInt -> CInt -> CDouble -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelBandwidthGet" 
    fdwf_analog_in_channel_bandwidth_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelBandwidthSet" 
    fdwf_analog_in_channel_bandwidth_set :: CInt -> CInt -> CDouble -> IO CInt

foreign import capi "dwf.h FDwfAnalogInChannelImpedanceGet" 
    fdwf_analog_in_channel_impedance_get :: CInt -> CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInChannelImpedanceSet"
    fdwf_analog_in_channel_impedance_set :: CInt -> CInt -> CDouble -> IO CInt

-- AI Trigger Configuration
foreign import capi "dwf.h FDwfAnalogInTriggerSourceSet"
    fdwf_analog_in_trigger_source_set :: CInt -> CUChar -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerSourceGet"
    fdwf_analog_in_trigger_source_get :: CInt -> Ptr CUChar -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerPositionInfo"
    fdwf_analog_in_trigger_position_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerPositionSet" 
    fdwf_analog_in_trigger_position_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerPositionGet" 
    fdwf_analog_in_trigger_position_get :: CInt -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerPositionStatus"
    fdwf_analog_in_trigger_position_status :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerAutoTimeoutInfo"
    fdwf_analog_in_trigger_auto_timeout_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerAutoTimeoutSet" 
    fdwf_analog_in_trigger_auto_timeout_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerAutoTimeoutGet" 
    fdwf_analog_in_trigger_auto_timeout_get :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerHoldOffInfo"
    fdwf_analog_in_trigger_hold_off_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerHoldOffSet" 
    fdwf_analog_in_trigger_hold_off_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerHoldOffGet" 
    fdwf_analog_in_trigger_hold_off_get :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerTypeInfo"
    fdwf_analog_in_trigger_type_info :: CInt -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerTypeSet" 
    fdwf_analog_in_trigger_type_set :: CInt -> CInt-> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerTypeGet" 
    fdwf_analog_in_trigger_type_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerChannelInfo"
    fdwf_analog_in_trigger_channel_info :: CInt -> Ptr (CInt) -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerChannelSet" 
    fdwf_analog_in_trigger_channel_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerChannelGet" 
    fdwf_analog_in_trigger_channel_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerFilterInfo"
    fdwf_analog_in_trigger_filter_info :: CInt -> Ptr (CInt) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerFilterSet" 
    fdwf_analog_in_trigger_filter_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerFilterGet" 
    fdwf_analog_in_trigger_filter_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerLevelInfo"
    fdwf_analog_in_trigger_level_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLevelSet" 
    fdwf_analog_in_trigger_level_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLevelGet" 
    fdwf_analog_in_trigger_level_get :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerHysteresisInfo"
    fdwf_analog_in_trigger_hysteresis_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerHysteresisSet" 
    fdwf_analog_in_trigger_hysteresis_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerHysteresisGet" 
    fdwf_analog_in_trigger_hysteresis_get :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerConditionInfo"
    fdwf_analog_in_trigger_condition_info :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerConditionSet" 
    fdwf_analog_in_trigger_condition_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerConditionGet" 
    fdwf_analog_in_trigger_condition_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerLengthInfo"
    fdwf_analog_in_trigger_length_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLengthSet" 
    fdwf_analog_in_trigger_length_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLengthGet" 
    fdwf_analog_in_trigger_length_get :: CInt -> Ptr (CDouble) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInTriggerLengthConditionInfo"
    fdwf_analog_in_trigger_length_condition_info :: CInt -> Ptr CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLengthConditionSet" 
    fdwf_analog_in_trigger_length_condition_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInTriggerLengthConditionGet" 
    fdwf_analog_in_trigger_length_condition_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInSamplingSourceSet" 
    fdwf_analog_in_sampling_source_set :: CInt -> CUChar -> IO CInt
foreign import capi "dwf.h FDwfAnalogInSamplingSourceGet" 
    fdwf_analog_in_sampling_source_get :: CInt -> Ptr (CUChar) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInSamplingSlopeSet" 
    fdwf_analog_in_sampling_slope_set :: CInt -> CInt -> IO CInt
foreign import capi "dwf.h FDwfAnalogInSamplingSlopeGet" 
    fdwf_analog_in_sampling_slope_get :: CInt -> Ptr (CInt) -> IO CInt

foreign import capi "dwf.h FDwfAnalogInSamplingDelaySet" 
    fdwf_analog_in_sampling_delay_set :: CInt -> CDouble -> IO CInt
foreign import capi "dwf.h FDwfAnalogInSamplingDelayGet" 
    fdwf_analog_in_sampling_delay_get :: CInt -> Ptr (CDouble) -> IO CInt

-- Analog IO

foreign import capi "dwf.h FDwfAnalogIOReset"
    fdwf_analog_io_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOConfigure"
    fdwf_analog_io_configure :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOStatus"
    fdwf_analog_io_status :: CInt -> IO (CInt)

foreign import capi "dwf.h FDwfAnalogIOEnableInfo"
    fdwf_analog_io_enable_info :: CInt -> Ptr (CInt) -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOEnableSet"
    fdwf_analog_io_enable_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOEnableGet"
    fdwf_analog_io_enable_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOEnableStatus"
    fdwf_analog_io_enable_status :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelCount"
    fdwf_analog_io_channel_count :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelName"
    fdwf_analog_io_channel_name :: CInt -> CInt-> CString -> CString -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelInfo"
    fdwf_analog_io_channel_info :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeName"
    fdwf_analog_io_channel_node_name :: CInt -> CInt -> CInt -> CString -> CString -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeInfo"
    fdwf_analog_io_channel_node_info :: CInt -> CInt -> CInt -> Ptr (CUChar) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeSetInfo"
    fdwf_analog_io_channel_node_set_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeSet"
    fdwf_analog_io_channel_node_set :: CInt -> CInt -> CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeGet"
    fdwf_analog_io_channel_node_get :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeStatusInfo"
    fdwf_analog_io_channel_node_status_info :: CInt -> CInt -> CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfAnalogIOChannelNodeStatus"
    fdwf_analog_io_channel_node_status :: CInt -> CInt -> CInt -> Ptr (CDouble) -> IO (CInt)

-- Digital IO

foreign import capi "dwf.h FDwfDigitalIOReset"
    fdwf_digital_io_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOConfigure"
    fdwf_digital_io_configure :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOStatus"
    fdwf_digital_io_status :: CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalIOOutputEnableInfo"
    fdwf_digital_io_output_enable_info :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputEnableSet"
    fdwf_digital_io_output_enable_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputEnableGet"
    fdwf_digital_io_output_enable_get :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputInfo"
    fdwf_digital_io_output_info :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputSet"
    fdwf_digital_io_output_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputGet"
    fdwf_digital_io_output_get :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOInputInfo"
    fdwf_digital_io_input_info :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOInputStatus"
    fdwf_digital_io_input_status :: CInt -> Ptr CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputEnableInfo64"
    fdwf_digital_io_output_enable_info64 :: CInt -> Ptr CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputEnableSet64"
    fdwf_digital_io_output_enable_set64 :: CInt -> CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputEnableGet64"
    fdwf_digital_io_output_enable_get64 :: CInt -> Ptr CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputInfo64"
    fdwf_digital_io_output_info64 :: CInt -> Ptr CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOOutputSet64"
    fdwf_digital_io_output_set64 :: CInt -> CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOInputInfo64"
    fdwf_digital_io_input_info64 :: CInt -> Ptr CULLong -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalIOInputStatus64"
    fdwf_digital_io_input_status64 :: CInt -> Ptr CULLong -> IO (CInt)

-- Digital In

foreign import capi "dwf.h FDwfDigitalInReset"
    fdwf_digital_in_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInConfigure"
    fdwf_digital_in_configure :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatus"
    fdwf_digital_in_status :: CInt -> CInt -> Ptr (CUChar) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusSamplesLeft"
    fdwf_digital_in_status_samples_left :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusSamplesValid"
    fdwf_digital_in_status_samples_valid :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusIndexWrite"
    fdwf_digital_in_status_index_write :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusAutoTriggered"
    fdwf_digital_in_status_auto_triggered :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusData"
    fdwf_digital_in_status_data :: CInt -> Ptr () -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusData2"
    fdwf_digital_in_status_data2 :: CInt -> Ptr () -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusNoise2"
    fdwf_digital_in_status_noise2 :: CInt -> Ptr () -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusRecord"
    fdwf_digital_in_status_record :: CInt -> Ptr (CInt) -> Ptr (CInt) -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInStatusTime"
    fdwf_digital_in_status_time :: CInt -> Ptr (CUInt) -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalInInternalClockInfo"
    fdwf_digital_in_internal_clock_info :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInClockSourceInfo"
    fdwf_digital_in_clock_source_info :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInClockSourceSet"
    fdwf_digital_in_clock_source_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInClockSourceGet"
    fdwf_digital_in_clock_source_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInDividerInfo"
    fdwf_digital_in_divider_info :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInDividerSet"
    fdwf_digital_in_divider_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInDividerGet"
    fdwf_digital_in_divider_get :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInBitsInfo"
    fdwf_digital_in_bits_info :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleFormatSet"
    fdwf_digital_in_sample_format_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleFormatGet"
    fdwf_digital_in_sample_format_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInInputOrderSet"
    fdwf_digital_in_input_order_set :: CInt -> CBool -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInBufferSizeInfo"
    fdwf_digital_in_buffer_size_info :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInBufferSizeSet"
    fdwf_digital_in_buffer_size_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInBufferSizeGet"
    fdwf_digital_in_buffer_size_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleModeInfo"
    fdwf_digital_in_sample_mode_info :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleModeSet"
    fdwf_digital_in_sample_mode_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleModeGet"
    fdwf_digital_in_sample_mode_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleSensibleSet"
    fdwf_digital_in_sample_sensible_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInSampleSensibleGet"
    fdwf_digital_in_sample_sensible_get :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInAcquisitionModeInfo"
    fdwf_digital_in_acquisition_mode_info :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInAcquisitionModeSet"
    fdwf_digital_in_acquisition_mode_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInAcquisitionModeGet"
    fdwf_digital_in_acquisition_mode_get :: CInt -> Ptr (CInt) -> IO (CInt)

-- Digital In Trigger Configuration

foreign import capi "dwf.h FDwfDigitalInTriggerSourceSet"
    fdwf_digital_in_trigger_source_set :: CInt -> CUChar -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerSourceGet"
    fdwf_digital_in_trigger_source_get :: CInt -> Ptr (CUChar) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerSlopeSet"
    fdwf_digital_in_trigger_slope_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerSlopeGet"
    fdwf_digital_in_trigger_slope_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerPositionInfo"
    fdwf_digital_in_trigger_position_info :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerPositionSet"
    fdwf_digital_in_trigger_position_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerPositionGet"
    fdwf_digital_in_trigger_position_get :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerPrefillSet"
    fdwf_digital_in_trigger_prefill_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerPrefillGet"
    fdwf_digital_in_trigger_prefill_get :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerAutoTimeoutInfo"
    fdwf_digital_in_trigger_auto_timeout_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerAutoTimeoutSet"
    fdwf_digital_in_trigger_auto_timeout_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerAutoTimeoutGet"
    fdwf_digital_in_trigger_auto_timeout_get :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerInfo"
    fdwf_digital_in_trigger_info :: CInt -> Ptr (CUInt) -> Ptr (CUInt) -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerSet"
    fdwf_digital_in_trigger_set :: CInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerGet"
    fdwf_digital_in_trigger_get :: CInt -> Ptr (CUInt) -> Ptr (CUInt) -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerResetSet"
    fdwf_digital_in_trigger_reset_set :: CInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerCountSet"
    fdwf_digital_in_trigger_count_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerLengthSet"
    fdwf_digital_in_trigger_length_set :: CInt -> CDouble -> CDouble -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalInTriggerMatchSet"
    fdwf_digital_in_trigger_match_set :: CInt -> CInt -> CUInt -> CUInt -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalOutReset"
    fdwf_digital_out_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutConfigure"
    fdwf_digital_out_configure :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutStatus"
    fdwf_digital_out_status :: CInt -> Ptr (CUChar) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutInternalClockInfo"
    fdwf_digital_out_internal_clock_info :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTriggerSourceSet"
    fdwf_digital_out_trigger_source_set :: CInt -> CUChar -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTriggerSourceGet"
    fdwf_digital_out_trigger_source_get :: CInt -> Ptr (CUChar) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRunInfo"
    fdwf_digital_out_run_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRunSet"
    fdwf_digital_out_run_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRunGet"
    fdwf_digital_out_run_get :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRunStatus"
    fdwf_digital_out_run_status :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutWaitInfo"
    fdwf_digital_out_wait_info :: CInt -> Ptr (CDouble) -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutWaitSet"
    fdwf_digital_out_wait_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutWaitGet"
    fdwf_digital_out_wait_get :: CInt -> Ptr (CDouble) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatInfo"
    fdwf_digital_out_repeat_info :: CInt -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatSet"
    fdwf_digital_out_repeat_set :: CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatGet"
    fdwf_digital_out_repeat_get :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatStatus"
    fdwf_digital_out_repeat_status :: CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTriggerSlopeSet"
    fdwf_digital_out_trigger_slope_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTriggerSlopeGet"
    fdwf_digital_out_trigger_slope_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatTriggerSet"
    fdwf_digital_out_repeat_trigger_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutRepeatTriggerGet"
    fdwf_digital_out_repeat_trigger_get :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCount"
    fdwf_digital_out_count :: CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutEnableSet"
    fdwf_digital_out_enable_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutEnableGet"
    fdwf_digital_out_enable_get :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutOutputInfo"
    fdwf_digital_out_output_info :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutOutputSet"
    fdwf_digital_out_output_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutOutputGet"
    fdwf_digital_out_output_get :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTypeInfo"
    fdwf_digital_out_type_info :: CInt -> CInt  -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTypeSet"
    fdwf_digital_out_type_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutTypeGet"
    fdwf_digital_out_type_get :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutIdleInfo"
    fdwf_digital_out_idle_info :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutIdleSet"
    fdwf_digital_out_idle_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutIdleGet"
    fdwf_digital_out_idle_get :: CInt -> CInt -> Ptr (CInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDividerInfo"
    fdwf_digital_out_divider_info :: CInt -> CInt -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDividerInitSet"
    fdwf_digital_out_divider_init_set :: CInt -> CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDividerInitGet"
    fdwf_digital_out_divider_init_get :: CInt -> CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDividerSet"
    fdwf_digital_out_divider_set :: CInt -> CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDividerGet"
    fdwf_digital_out_divider_get :: CInt -> CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCounterInfo"
    fdwf_digital_out_counter_info :: CInt -> CInt -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCounterInitSet"
    fdwf_digital_out_counter_init_set :: CInt -> CInt -> CUInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCounterInitGet"
    fdwf_digital_out_counter_init_get :: CInt -> CInt -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCounterSet"
    fdwf_digital_out_counter_set :: CInt -> CInt -> CUInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutCounterGet"
    fdwf_digital_out_counter_get :: CInt -> CInt -> Ptr (CUInt) -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDataInfo"
    fdwf_digital_out_data_info :: CInt -> CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutDataSet"
    fdwf_digital_out_data_set :: CInt -> CInt -> Ptr (CUChar) -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutPlayDataSet"
    fdwf_digital_out_play_data_set :: CInt -> Ptr CUChar -> CUInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalOutPlayRateSet"
    fdwf_digital_out_play_rate_set :: CInt -> CDouble -> IO (CInt)

-- UART

foreign import capi "dwf.h FDwfDigitalUartReset"
    fdwf_digital_uart_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartRateSet"
    fdwf_digital_uart_rate_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartBitsSet"
    fdwf_digital_uart_bits_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartParitySet"
    fdwf_digital_uart_parity_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartStopSet"
    fdwf_digital_uart_stop_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartTxSet"
    fdwf_digital_uart_tx_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartRxSet"
    fdwf_digital_uart_rx_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartTx"
    fdwf_digital_uart_tx :: CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalUartRx"
    fdwf_digital_uart_rx :: CInt -> Ptr (CUChar) -> CInt -> Ptr (CInt) -> Ptr (CInt) -> IO (CInt)

-- SPI

foreign import capi "dwf.h FDwfDigitalSpiReset"
    fdwf_digital_spi_reset :: CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiFrequencySet"
    fdwf_digital_spi_frequency_set :: CInt -> CDouble -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiClockSet"
    fdwf_digital_spi_clock_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiDataSet"
    fdwf_digital_spi_data_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiIdleSet"
    fdwf_digital_spi_idle_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiModeSet"
    fdwf_digital_spi_mode_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiOrderSet"
    fdwf_digital_spi_order_set :: CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiDelaySet"
    fdwf_digital_spi_delay_set :: CInt -> CInt -> CInt -> CInt -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalSpiSelectSet"
    fdwf_digital_spi_select_set :: CInt -> CInt -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiSelect"
    fdwf_digital_spi_select :: CInt -> CInt -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalSpiWriteRead"
    fdwf_digital_spi_write_read :: CInt -> CInt -> CInt-> Ptr (CUChar) -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiWriteRead16"
    fdwf_digital_spi_write_read16 :: CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiWriteRead32"
    fdwf_digital_spi_write_read32 :: CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalSpiReadOne"
    fdwf_digital_spi_read_one :: CInt -> CInt -> CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiRead"
    fdwf_digital_spi_read :: CInt -> CInt -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiRead16"
    fdwf_digital_spi_read16 :: CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiRead32"
    fdwf_digital_spi_read32 :: CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalSpiWriteOne"
    fdwf_digital_spi_write_one :: CInt -> CInt -> CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiWrite"
    fdwf_digital_spi_write :: CInt -> CInt -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiWrite16"
    fdwf_digital_spi_write16 :: CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiWrite32"
    fdwf_digital_spi_write32 :: CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)

foreign import capi "dwf.h FDwfDigitalSpiCmdWriteRead"
    fdwf_digital_spi_cmd_write_read :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUChar) -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWriteRead16"
    fdwf_digital_spi_cmd_write_read16 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWriteRead32"
    fdwf_digital_spi_cmd_write_read32 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWriteOne"
    fdwf_digital_spi_cmd_write_one :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> CUInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWrite"
    fdwf_digital_spi_cmd_write :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWrite16"
    fdwf_digital_spi_cmd_write16 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdWrite32"
    fdwf_digital_spi_cmd_write32 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdReadOne"
    fdwf_digital_spi_cmd_read_one :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUInt) -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdRead"
    fdwf_digital_spi_cmd_read :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUChar) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdRead16"
    fdwf_digital_spi_cmd_read16 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUShort) -> CInt -> IO (CInt)
foreign import capi "dwf.h FDwfDigitalSpiCmdRead32"
    fdwf_digital_spi_cmd_read32 :: CInt -> CInt -> CUChar -> CInt -> CInt -> CInt -> Ptr (CUInt) -> CInt -> IO (CInt)

-- CAN
-- SWD
