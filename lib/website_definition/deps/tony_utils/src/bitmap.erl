%% @doc
%% Provides a bitmap abstraction.
%%
%% First variation allows bitmap to be
%% distributed across multiple processes,
%% with each process being responsible for a portion
%% of the bitmap.  This in turn allows efficient
%% SMP processing, and smaller state within processes.
%% @end
-module (bitmap).
-export([bit_set/3,bit_read/2,bit_array/2,
   bit_array/3,bit_test/0,
   valid_position/2]).

%% Prime number array is a binary
%% 
-type bit() :: 0|1.
-type bitmap() :: binary() | {First::integer(),Size::integer(),bitmap()}.
-spec bit_set(bitmap(),integer()|[integer()],bit()) -> bitmap().

valid_position({First,Size,_},BitPosition) ->
  (BitPosition >= First) and (BitPosition < First+Size).

bit_set(Data,PosArray,Value) when is_list(PosArray) ->
  lists:foldl(
    fun (X,Acc) -> bit_set(Acc,X,Value) end,
    Data,PosArray);
  
bit_set({First,Size,Data},BitPosition,Value) ->
  {First,Size,bit_set1(Data,(BitPosition-First),Value)}.

bit_set1(Data,BitPosition,Value)
     when is_binary(Data) and is_integer(BitPosition) ->
  NumBytes = size(Data),
  ByteNo = (BitPosition div 8) + 1,
  HasLeftBytes = (ByteNo > 1),
  HasRightBytes = (ByteNo<NumBytes),
  case ByteNo =< NumBytes of
    true ->
      LeftCnt = ByteNo - 1,
      RightCnt = NumBytes - ByteNo,
      say("LeftCnt: ~p, RightCnt: ~p",[LeftCnt,RightCnt]),
      <<Left:LeftCnt/bytes,Target:1/bytes,Right:RightCnt/bytes>> = Data,
      BitNo = (BitPosition rem 8),
      BitVal = 1 bsl BitNo,
      true = (BitVal < 256),
      [T2] = binary_to_list(Target),
      say("Target~p",[Target]),
      case Value of
        0 -> NT = T2 band (bnot BitVal);
        1 -> NT = T2 bor BitVal
      end,
      T2b = list_to_binary([NT]),
      say("yy",[]),
      if
        (HasLeftBytes == false) and (HasRightBytes == false) ->
          <<T2b:1/bytes>>;
        (HasLeftBytes == true)  and (HasRightBytes == false) ->
          <<Left:LeftCnt/bytes,T2b:1/bytes>>;
        (HasLeftBytes == false) and (HasRightBytes == true ) ->        
          <<T2b:1/bytes,Right:RightCnt/bytes>>;
        (HasLeftBytes == true)  and (HasRightBytes == true ) ->        
          <<Left:LeftCnt/bytes,T2b:1/bytes,Right:RightCnt/bytes>>
      end;
    false ->
      Data
  end.

-spec bit_read(bitmap(),integer()) -> bit().
bit_read({First,_Size,Data},BitPosition) ->
  say("bit_read First read: First=~p, BitPosition=~p",
    [First,BitPosition]),
  bit_read1(Data,BitPosition-First).
bit_read1(Data,BitPosition) 
     when is_binary(Data) and is_integer(BitPosition) ->
  say("bit_read(~p,~p)",[Data,BitPosition]),
  NumBytes = size(Data),
  ByteNo = (BitPosition div 8)+1,
  LeftCnt = ByteNo - 1,
  RightCnt = NumBytes - ByteNo,
  true = (ByteNo =< NumBytes),
  BitNo = BitPosition rem 8,
  BitVal = 1 bsl BitNo,
  <<_:LeftCnt/bytes,Target:1/bytes,_:RightCnt/bytes>> = Data,
  [N2] = binary_to_list(Target),
  case (N2 band BitVal) > 0 of
    true  -> 1;
    false -> 0
  end.

-spec bit_array(First::integer,Size::integer(),Default::bit()) -> bitmap().
bit_array(First,Size,Default) ->
  {First,Size,bit_array1(Size,Default)}.
-spec bit_array(integer(),bit()) -> bitmap().
bit_array(Bits,Default) ->
  bit_array(0,Bits,Default).

bit_array1(Bits,Default) 
     when Bits > 1 ->
  Bytes = (Bits div 8) + 1,
  Fill = 
    case Default of
      0 -> 0;
      1 -> 255
    end,
  list_to_binary(
    lists:map(fun (_) -> Fill end, lists:seq(1,Bytes))).

zero_filled() ->
  TenZeroFilled = {0,10,<<0,0>>} = bit_array(10,0),
  BitsSet = [1,7,3,9,5,3],
  ZZ = bit_set(TenZeroFilled,BitsSet,1),
  TestList = lists:seq(1,10),
  [1,0,1,0,1,0,1,0,1,0] 
    = lists:map(fun (X) -> bit_read(ZZ,X) end, 
    TestList),
  ok.

offset_test() ->
  Plus100 = {100,10,<<0,0>>} = bit_array(100,10,0),
  BitsSet = [X+100 || X <- [1,7,3,9,5,3]],
  ZZ = {100,10,_} = bit_set(Plus100,BitsSet,1),
  TestList = lists:seq(100,109),
  [0,1,0,1,0,1,0,1,0,1] 
    = lists:map(fun (X) -> bit_read(ZZ,X) end, 
    TestList),
  ok.
  
bit_test() ->
  ok=zero_filled(),
  ok=offset_test().

say(_Format, _Data) ->
  %io:format(
  %  "~p:~p: ~s~n", 
  %   [?MODULE, self(), io_lib:format(_Format, _Data)]),
  ok.
