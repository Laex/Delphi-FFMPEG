unit uFFComponentLink;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Helpers for published TComponent link properties (FreeNotification). }

interface

uses
  {$IFDEF FPC}
  Classes;
  {$ELSE}
  System.Classes;
  {$ENDIF}

procedure FFSetLinkedComponent(AOwner: TComponent; var ALink: TComponent; const AValue: TComponent);

function FFHandleLinkRemoval(var ALink: TComponent; AComponent: TComponent; Operation: TOperation): Boolean;

implementation

procedure FFSetLinkedComponent(AOwner: TComponent; var ALink: TComponent; const AValue: TComponent);
begin
  if ALink = AValue then
    Exit;
  if Assigned(ALink) then
    ALink.RemoveFreeNotification(AOwner);
  ALink := AValue;
  if Assigned(ALink) then
    ALink.FreeNotification(AOwner);
end;

function FFHandleLinkRemoval(var ALink: TComponent; AComponent: TComponent; Operation: TOperation): Boolean;
begin
  Result := False;
  if (Operation = opRemove) and (AComponent = ALink) then
  begin
    ALink := nil;
    Result := True;
  end;
end;

end.
