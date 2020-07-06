<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis labelsEnabled="0" readOnly="0" simplifyDrawingTol="1" simplifyLocal="1" styleCategories="AllStyleCategories" version="3.8.3-Zanzibar" simplifyDrawingHints="1" simplifyAlgorithm="0" maxScale="0" hasScaleBasedVisibilityFlag="0" minScale="1e+08" simplifyMaxScale="1">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <renderer-v2 symbollevels="0" enableorderby="0" type="singleSymbol" forceraster="0">
    <symbols>
      <symbol clip_to_extent="1" force_rhr="0" type="line" alpha="1" name="0">
        <layer class="ArrowLine" pass="0" locked="0" enabled="1">
          <prop k="arrow_start_width" v="0.05"/>
          <prop k="arrow_start_width_unit" v="MM"/>
          <prop k="arrow_start_width_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="arrow_type" v="0"/>
          <prop k="arrow_width" v="0.01"/>
          <prop k="arrow_width_unit" v="MM"/>
          <prop k="arrow_width_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="head_length" v="1"/>
          <prop k="head_length_unit" v="MM"/>
          <prop k="head_length_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="head_thickness" v="0.1"/>
          <prop k="head_thickness_unit" v="MM"/>
          <prop k="head_thickness_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="head_type" v="0"/>
          <prop k="is_curved" v="0"/>
          <prop k="is_repeated" v="1"/>
          <prop k="offset" v="0"/>
          <prop k="offset_unit" v="MM"/>
          <prop k="offset_unit_scale" v="3x:0,0,0,0,0,0"/>
          <prop k="ring_filter" v="0"/>
          <data_defined_properties>
            <Option type="Map">
              <Option value="" type="QString" name="name"/>
              <Option name="properties"/>
              <Option value="collection" type="QString" name="type"/>
            </Option>
          </data_defined_properties>
          <symbol clip_to_extent="1" force_rhr="0" type="fill" alpha="1" name="@0@0">
            <layer class="SimpleFill" pass="0" locked="0" enabled="1">
              <prop k="border_width_map_unit_scale" v="3x:0,0,0,0,0,0"/>
              <prop k="color" v="127,127,127,255"/>
              <prop k="joinstyle" v="bevel"/>
              <prop k="offset" v="0,0"/>
              <prop k="offset_map_unit_scale" v="3x:0,0,0,0,0,0"/>
              <prop k="offset_unit" v="MM"/>
              <prop k="outline_color" v="35,35,35,255"/>
              <prop k="outline_style" v="no"/>
              <prop k="outline_width" v="0.01"/>
              <prop k="outline_width_unit" v="MM"/>
              <prop k="style" v="solid"/>
              <data_defined_properties>
                <Option type="Map">
                  <Option value="" type="QString" name="name"/>
                  <Option name="properties"/>
                  <Option value="collection" type="QString" name="type"/>
                </Option>
              </data_defined_properties>
            </layer>
          </symbol>
        </layer>
      </symbol>
    </symbols>
    <rotation/>
    <sizescale/>
  </renderer-v2>
  <customproperties>
    <property value="0" key="embeddedWidgets/count"/>
    <property key="variableNames"/>
    <property key="variableValues"/>
  </customproperties>
  <blendMode>0</blendMode>
  <featureBlendMode>0</featureBlendMode>
  <layerOpacity>1</layerOpacity>
  <SingleCategoryDiagramRenderer diagramType="Histogram" attributeLegend="1">
    <DiagramCategory labelPlacementMethod="XHeight" sizeScale="3x:0,0,0,0,0,0" backgroundAlpha="255" minScaleDenominator="0" height="15" opacity="1" backgroundColor="#ffffff" penColor="#000000" scaleBasedVisibility="0" sizeType="MM" diagramOrientation="Up" lineSizeScale="3x:0,0,0,0,0,0" penWidth="0" penAlpha="255" barWidth="5" width="15" lineSizeType="MM" scaleDependency="Area" minimumSize="0" rotationOffset="270" maxScaleDenominator="1e+08" enabled="0">
      <fontProperties description="MS Shell Dlg 2,8.25,-1,5,50,0,0,0,0,0" style=""/>
      <attribute field="" color="#000000" label=""/>
    </DiagramCategory>
  </SingleCategoryDiagramRenderer>
  <DiagramLayerSettings showAll="1" obstacle="0" dist="0" placement="2" linePlacementFlags="18" zIndex="0" priority="0">
    <properties>
      <Option type="Map">
        <Option value="" type="QString" name="name"/>
        <Option name="properties"/>
        <Option value="collection" type="QString" name="type"/>
      </Option>
    </properties>
  </DiagramLayerSettings>
  <geometryOptions geometryPrecision="0" removeDuplicateNodes="0">
    <activeChecks/>
    <checkConfiguration/>
  </geometryOptions>
  <fieldConfiguration>
    <field name="tide_number">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="begin">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
    <field name="end">
      <editWidget type="Range">
        <config>
          <Option/>
        </config>
      </editWidget>
    </field>
  </fieldConfiguration>
  <aliases>
    <alias field="tide_number" index="0" name=""/>
    <alias field="begin" index="1" name=""/>
    <alias field="end" index="2" name=""/>
  </aliases>
  <excludeAttributesWMS/>
  <excludeAttributesWFS/>
  <defaults>
    <default field="tide_number" expression="" applyOnUpdate="0"/>
    <default field="begin" expression="" applyOnUpdate="0"/>
    <default field="end" expression="" applyOnUpdate="0"/>
  </defaults>
  <constraints>
    <constraint field="tide_number" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="begin" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
    <constraint field="end" notnull_strength="0" constraints="0" exp_strength="0" unique_strength="0"/>
  </constraints>
  <constraintExpressions>
    <constraint desc="" field="tide_number" exp=""/>
    <constraint desc="" field="begin" exp=""/>
    <constraint desc="" field="end" exp=""/>
  </constraintExpressions>
  <expressionfields/>
  <attributeactions>
    <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
  </attributeactions>
  <attributetableconfig actionWidgetStyle="dropDown" sortOrder="0" sortExpression="">
    <columns>
      <column hidden="0" type="field" width="-1" name="tide_number"/>
      <column hidden="0" type="field" width="-1" name="begin"/>
      <column hidden="0" type="field" width="-1" name="end"/>
      <column hidden="1" type="actions" width="-1"/>
    </columns>
  </attributetableconfig>
  <conditionalstyles>
    <rowstyles/>
    <fieldstyles/>
  </conditionalstyles>
  <editform tolerant="1"></editform>
  <editforminit/>
  <editforminitcodesource>0</editforminitcodesource>
  <editforminitfilepath></editforminitfilepath>
  <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
	geom = feature.geometry()
	control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
  <featformsuppress>0</featformsuppress>
  <editorlayout>generatedlayout</editorlayout>
  <editable>
    <field name="begin" editable="1"/>
    <field name="end" editable="1"/>
    <field name="tide_number" editable="1"/>
  </editable>
  <labelOnTop>
    <field labelOnTop="0" name="begin"/>
    <field labelOnTop="0" name="end"/>
    <field labelOnTop="0" name="tide_number"/>
  </labelOnTop>
  <widgets/>
  <previewExpression>tide_number</previewExpression>
  <mapTip></mapTip>
  <layerGeometryType>1</layerGeometryType>
</qgis>
